local wezterm = require 'wezterm'

local config = wezterm.config_builder()

--------------------------------------------------
-- BASIC CONFIGURATION
--------------------------------------------------
config.automatically_reload_config = true
config.scrollback_lines = 10000

--------------------------------------------------
-- APPEARANCE SETTINGS
--------------------------------------------------
-- Color scheme
config.color_scheme = 'nordfox'

-- Window appearance
config.window_decorations = "RESIZE"
config.window_padding = {
  left = 20,
  right = 20,
  top = 5,
  bottom = 0,
}
config.enable_scroll_bar = true

-- Cursor styling
config.cursor_blink_rate = 800
config.default_cursor_style = 'SteadyBar'

--------------------------------------------------
-- FONT CONFIGURATION
--------------------------------------------------
config.font = wezterm.font 'Pragmasevka Nerd Font'
config.font_size = 16
config.freetype_load_target = "Light"
config.freetype_render_target = "HorizontalLcd"

--------------------------------------------------
-- PERFORMANCE SETTINGS
--------------------------------------------------
config.front_end = "WebGpu"

--------------------------------------------------
-- TAB BAR CUSTOMIZATION
--------------------------------------------------
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true

-- Tab title formatting function
function tab_title(tab_info)
  local title = tab_info.tab_title
  if title and #title > 0 then
    return title
  end
  return tab_info.active_pane.title
end

-- Custom tab styling
wezterm.on(
  'format-tab-title',
  function(tab, tabs, panes, config, hover, max_width)
    -- Nordfox palette colors
    local background = '#3b4252'       -- Nord slightly lighter background
    local foreground = '#d8dee9'       -- Nord light foreground

    -- Indicator for active tab (can be a dot, circle, or other symbol)
    local indicator = '  '

    if tab.is_active then
      background = '#4c566a'           -- Nord darkest highlight
      foreground = '#eceff4'           -- Nord brightest foreground
      indicator = '• '                 -- Active tab indicator
    elseif hover then
      background = '#434c5e'           -- Nord medium background
      foreground = '#e5e9f0'           -- Nord medium foreground
    end

    local title = tab_title(tab)
    -- Add extra spaces for padding
    title = ' ' .. indicator .. title .. ' '

    -- Calculate available width and truncate if needed (accounting for padding)
    local available_width = max_width
    title = wezterm.truncate_right(title, available_width)

    return {
      { Background = { Color = background } },
      { Foreground = { Color = foreground } },
      { Text = title },
    }
  end
)

--------------------------------------------------
-- KEYBINDINGS
--------------------------------------------------
config.keys = {
   -- Disable default assignments
   {
      key = 'm',
      mods = 'CMD',
      action = wezterm.action.DisableDefaultAssignment,
   },
   {
      key = 'Enter',
      mods = 'OPT',
      action = wezterm.action.DisableDefaultAssignment,
   },
   {
      key = '_',
      mods = 'CTRL|SHIFT',
      action = wezterm.action.DisableDefaultAssignment,
   },

   -- Configuration management
   {
      key = 'r',
      mods = 'CMD|SHIFT',
      action = wezterm.action.ReloadConfiguration,
   },
}

--------------------------------------------------
-- WINDOW BEHAVIOR
--------------------------------------------------
config.initial_cols = 140
config.initial_rows = 40
config.adjust_window_size_when_changing_font_size = false

--------------------------------------------------
-- TERMINAL FEATURES
--------------------------------------------------
config.enable_kitty_keyboard = true
config.selection_word_boundary = " \t\n{}[]()\"'`,;:"

return config
