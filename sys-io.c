#include <xcb/xcb.h>

#include <xkbcommon/xkbcommon.h>
#include <xkbcommon/xkbcommon-x11.h>

#include <cairo/cairo.h>
#include <cairo/cairo-xcb.h>

#include <stdio.h>
#include <stdlib.h>

enum event_types{
	NONE,
	EXPOSE,
	BUTTON_LEFT_PRESS,
	BUTTON_MIDDLE_PRESS,
	BUTTON_RIGHT_PRESS,
	SCROLL_DOWN,
	SCROLL_UP,
	KEY_PRESS,
	MOTION,
	RESIZE,
	QUIT
};

enum event_states{
	BUTTON_LEFT = 1,
	BUTTON_MIDDLE = 2,
	BUTTON_RIGHT = 4,
	CONTROL = 8
};

enum special_keys{
	BACKSPACE = 0x1FF,
	LEFT = 0x2FF,
	RIGHT = 0x3FF,
	UP = 0x4FF,
	DOWN = 0x5FF
};

typedef struct{
	uint16_t width;
	uint16_t height;

	xcb_connection_t *connection;
	xcb_screen_t *screen;
	xcb_window_t win;
	xcb_atom_t delete_window;

	cairo_surface_t *surface;
	cairo_t *cr;
	uint8_t glyph_width;
	uint8_t glyph_height;

	struct xkb_context *ctx;
	struct xkb_keymap *keymap;
	struct xkb_state *state;
} window_t;

static uint8_t
init_xcb(window_t *window, uint16_t width, uint16_t height){
	xcb_connection_t *connection;
	xcb_screen_t *screen;
	uint32_t event_mask;
	xcb_window_t xcb_window;
	xcb_intern_atom_cookie_t protocols_cookie, delete_window_cookie;
	xcb_intern_atom_reply_t *protocols_reply, *delete_window_reply;

	connection = xcb_connect(NULL, NULL);
	if(!connection){
		printf("ERROR: Failed to connect to the X server.\n");
		return 0;
	}
	screen = xcb_setup_roots_iterator(xcb_get_setup(connection)).data;
	event_mask = XCB_EVENT_MASK_EXPOSURE         |
	             XCB_EVENT_MASK_BUTTON_PRESS     |
	             XCB_EVENT_MASK_POINTER_MOTION   |
	             XCB_EVENT_MASK_KEY_PRESS        |
	             XCB_EVENT_MASK_STRUCTURE_NOTIFY;
	xcb_window = xcb_generate_id(connection);
	xcb_create_window(
		connection,
		XCB_COPY_FROM_PARENT,
		xcb_window,
		screen->root,
		0, 0,
		width, height,
		0,
		XCB_WINDOW_CLASS_INPUT_OUTPUT,
		screen->root_visual,
		XCB_CW_EVENT_MASK, &event_mask
	);
	xcb_map_window(connection, xcb_window);
	xcb_flush(connection);
	protocols_cookie = xcb_intern_atom(connection, 1, 12, "WM_PROTOCOLS");
	delete_window_cookie = xcb_intern_atom(connection, 0, 16, "WM_DELETE_WINDOW");
	protocols_reply = xcb_intern_atom_reply(connection, protocols_cookie, NULL);
	delete_window_reply = xcb_intern_atom_reply(connection, delete_window_cookie, NULL);
	xcb_change_property(
		connection,
		XCB_PROP_MODE_REPLACE,
		xcb_window,
		protocols_reply->atom,
		XCB_ATOM_ATOM,
		32, 1,
		&delete_window_reply->atom
	);
	window->delete_window = delete_window_reply->atom;
	free(protocols_reply);
	free(delete_window_reply);
	window->connection = connection;
	window->screen = screen;
	window->win = xcb_window;
	return 1;
}

static xcb_visualtype_t *
get_visualtype(xcb_screen_t *screen){
	xcb_depth_iterator_t depth_iter;
	xcb_visualtype_iterator_t visualtype_iter;

	depth_iter = xcb_screen_allowed_depths_iterator(screen);
	for(; depth_iter.rem; xcb_depth_next(&depth_iter)){
		visualtype_iter = xcb_depth_visuals_iterator(depth_iter.data);
		for(; visualtype_iter.rem; xcb_visualtype_next(&visualtype_iter)){
			if(visualtype_iter.data->visual_id == screen->root_visual)
				return visualtype_iter.data;
		}
	}
	return NULL;
}

static uint8_t
px_to_pt(uint8_t px, xcb_screen_t *screen){
	double width_pts, px_per_pt;
	width_pts = screen->width_in_millimeters * 2.835;
	px_per_pt = screen->width_in_pixels / width_pts;
	return px * px_per_pt;
}

static void
init_cairo(window_t *window, uint16_t width, uint16_t height, const char *font, uint8_t font_size){
	xcb_visualtype_t *visualtype;
	cairo_surface_t *surface;
	cairo_t *cr;
	cairo_font_extents_t font_extents;

	visualtype = get_visualtype(window->screen);
	surface = cairo_xcb_surface_create(
		window->connection,
		window->win,
		visualtype,
		width,
		height
	);
	cr = cairo_create(surface);
	cairo_set_line_width(cr, 1);
	cairo_select_font_face(cr, font, CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
	cairo_set_font_size(cr, px_to_pt(font_size, window->screen));
	cairo_font_extents(cr, &font_extents);
	window->surface = surface;
	window->cr = cr;
	window->glyph_width = (uint8_t)font_extents.max_x_advance;
	window->glyph_height = (uint8_t)font_extents.height;
}

static void
init_xkb(window_t *window){
	struct xkb_context *ctx;
	int32_t device_id;
	struct xkb_keymap *keymap;
	struct xkb_state *state;

	ctx = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
	xkb_x11_setup_xkb_extension(window->connection, 1, 0, 0, NULL, NULL, NULL, NULL);
	device_id = xkb_x11_get_core_keyboard_device_id(window->connection);
	keymap = xkb_x11_keymap_new_from_device(
		ctx,
		window->connection,
		device_id,
		XKB_KEYMAP_COMPILE_NO_FLAGS
	);
	state = xkb_x11_state_new_from_device(keymap, window->connection, device_id);
	window->ctx = ctx;
	window->keymap = keymap;
	window->state = state;
}

window_t *
create_window(uint16_t width, uint16_t height, const char *font, uint8_t font_size){
	window_t *window;
	window = malloc(sizeof(window_t));
	window->width = width;
	window->height = height;
	if(!init_xcb(window, width, height)){
		free(window);
		return NULL;
	}
	init_cairo(window, width, height, font, font_size);
	init_xkb(window);
	return window;
}

void
destroy_window(window_t *window){
	cairo_surface_destroy(window->surface);
	cairo_destroy(window->cr);
	xkb_context_unref(window->ctx);
	xkb_keymap_unref(window->keymap);
	xkb_state_unref(window->state);
	xcb_destroy_window(window->connection, window->win);
	xcb_disconnect(window->connection);
	free(window);
}

uint16_t
window_width(window_t *window){
	return window->width;
}

uint16_t
window_height(window_t *window){
	return window->height;
}

uint8_t
glyph_width(window_t *window){
	return window->glyph_width;
}

uint8_t
glyph_height(window_t *window){
	return window->glyph_height;
}

void
display_window(window_t *window){
	cairo_surface_flush(window->surface);
	xcb_flush(window->connection);
}

void
set_color(window_t *window, uint8_t r, uint8_t g, uint8_t b){
	double r_scaled, g_scaled, b_scaled;
	r_scaled = (double)(r + 1) / 256;
	g_scaled = (double)(g + 1) / 256;
	b_scaled = (double)(b + 1) / 256;
	cairo_set_source_rgb(window->cr, r_scaled, g_scaled, b_scaled);
}

void
move_to(window_t *window, uint16_t x, uint16_t y){
	cairo_move_to(window->cr, x, y);
}

void
line_to(window_t *window, uint16_t x, uint16_t y){
	cairo_line_to(window->cr, x, y);
}

void
draw(window_t *window){
	cairo_stroke(window->cr);
}

void
fill_region(window_t *window){
	cairo_fill(window->cr);
}

void
clear_window(window_t *window){
	cairo_paint(window->cr);
}

void
print_string(window_t *window, const char *str){
	cairo_show_text(window->cr, str);
}

uint8_t
get_state(uint16_t xcb_state){
	uint8_t state = 0;
	if(xcb_state & XCB_BUTTON_MASK_1) state += BUTTON_LEFT;
	if(xcb_state & XCB_BUTTON_MASK_2) state += BUTTON_MIDDLE;
	if(xcb_state & XCB_BUTTON_MASK_3) state += BUTTON_RIGHT;
	if(xcb_state & XCB_MOD_MASK_CONTROL) state += CONTROL;
	return state;
}

static uint64_t
handle_button(xcb_button_press_event_t *event){
	uint64_t button, state, x, y;
	switch(event->detail){
		case 1:
			button = BUTTON_LEFT_PRESS;
			break;
		case 2:
			button = BUTTON_MIDDLE_PRESS;
			break;
		case 3:
			button = BUTTON_RIGHT_PRESS;
			break;
		case 4: return SCROLL_UP;
		case 5: return SCROLL_DOWN;
		default: return NONE;
	}
	state = get_state(event->state);
	x = event->event_x;
	y = event->event_y;
	return button + (state << 8) + (x << 16) + (y << 32);
}

static uint64_t
handle_key(xcb_key_press_event_t *event, window_t *window){
	xkb_mod_mask_t mask;
	xkb_keysym_t keysym;
	uint64_t state, key;

	if(event->state & XCB_MOD_MASK_SHIFT)
		mask = 1 << xkb_keymap_mod_get_index(window->keymap, XKB_MOD_NAME_SHIFT);
	else mask = 0;
	xkb_state_update_mask(window->state, mask, 0, 0, 0, 0, 0);
	keysym = xkb_state_key_get_one_sym(window->state, event->detail);
	if(keysym >= 32 && keysym <= 126) key = keysym;
	else{
		switch(keysym){
			case XKB_KEY_BackSpace:
				key = BACKSPACE;
				break;
			case XKB_KEY_Left:
				key = LEFT;
				break;
			case XKB_KEY_Right:
				key = RIGHT;
				break;
			case XKB_KEY_Up:
				key = UP;
				break;
			case XKB_KEY_Down:
				key = DOWN;
				break;
			case XKB_KEY_Return:
				key = (uint64_t)'\n';
				break;
			default: return 0;
		}
	}
	state = get_state(event->state);
	return KEY_PRESS + (state << 8) + (key << 16);
}

static uint64_t
handle_motion(xcb_motion_notify_event_t *event){
	uint64_t state, x, y;
	state = get_state(event->state);
	x = event->event_x;
	y = event->event_y;
	return MOTION + (state << 8) + (x << 16) + (y << 32);
}

static uint64_t
handle_resize(xcb_configure_notify_event_t *event, window_t *window){
	if(event->width == window->width || event->height == window->height)
		return NONE;
	window->width = event->width;
	window->height = event->height;
	return RESIZE;
}

uint64_t
get_event(window_t *window){
	xcb_generic_event_t *event;
	uint64_t event_code = NONE;

	event = xcb_wait_for_event(window->connection);
	switch(event->response_type & ~0x80){
		case XCB_EXPOSE:
			event_code = EXPOSE;
			break;
		case XCB_BUTTON_PRESS:
			event_code = handle_button((xcb_button_press_event_t *)event);
			break;
		case XCB_KEY_PRESS:
			event_code = handle_key((xcb_key_press_event_t *)event, window);
			break;
		case XCB_MOTION_NOTIFY:
			event_code = handle_motion((xcb_motion_notify_event_t *)event);
			break;
		case XCB_CONFIGURE_NOTIFY:
			event_code = handle_resize((xcb_configure_notify_event_t *)event, window);
			break;
		case XCB_CLIENT_MESSAGE:
			if(((xcb_client_message_event_t *)event)->data.data32[0] == window->delete_window){
				destroy_window(window);
				event_code = QUIT;
			}
			break;
	}
	free(event);
	return event_code;
}