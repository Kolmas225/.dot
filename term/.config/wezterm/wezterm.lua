local wezterm = require("wezterm")
local config = {}

config.default_prog = { "/usr/bin/env", "-S", "fish", "-i" }
config.warn_about_missing_glyphs = false
config.font = wezterm.font_with_fallback({
	"Maple Mono",
	"Chiron Hei HK",
})
config.font_size = 14.0
config.color_scheme = "Catppuccin Macchiato"

config.window_decorations = "NONE"
config.hide_tab_bar_if_only_one_tab = true

config.enable_kitty_keyboard = true

config.keys = {
	-- Restore C-i to tab
	{
		key = "i",
		mods = "CTRL",
		action = wezterm.action.SendKey({
			key = "Tab",
		}),
	},
	{
		key = "I",
		mods = "CTRL|SHIFT",
		action = wezterm.action.SendKey({
			key = "Tab",
			mods = "SHIFT",
		}),
	},
}

return config
