(* https://www.ibm.com/developerworks/library/wa-support-multiple-keyboard-layouts-in-web-based-vnc-apps-trs/index.html *)

type keysym = [
  (* see X11/keysymdef.h, /usr/share/X11/XKeysymDB *)
  | `System_Power
  | `System_Sleep
  | `System_Wake
  | `No_Event
  | `Overrun_Error
  | `POST_Fail
  | `ErrorUndefined
  | `A
  | `B
  | `C
  | `D
  | `E
  | `F
  | `G
  | `H
  | `I
  | `J
  | `K
  | `L
  | `M
  | `N
  | `O
  | `P
  | `Q
  | `R
  | `S
  | `T
  | `U
  | `V
  | `W
  | `X
  | `Y
  | `Z
  | `_1
  | `_2
  | `_3
  | `_4
  | `_5
  | `_6
  | `_7
  | `_8
  | `_9
  | `_0
  | `Return
  | `Escape
  | `Backspace
  | `Tab
  | `ISO_Left_Tab (*shift-tab, going left*)
  | `Space
  | `Minus
  | `Equals
  | `Leftbracket
  | `Rightbracket
  | `Backslash
  | `Europe_1
  | `Semicolon
  | `Apostrophe
  | `Grave
  | `Comma
  | `Period
  | `Slash
  | `Caps_Lock
  | `F1
  | `F2
  | `F3
  | `F4
  | `F5
  | `F6
  | `F7
  | `F8
  | `F9
  | `F10
  | `F11
  | `F12
  | `Print_Screen
  | `Scroll_Lock
  | `Break
  | `Pause
  | `Insert
  | `Home
  | `Page_Up
  | `Delete
  | `End
  | `Page_Down
  | `Right_Arrow
  | `Left_Arrow
  | `Down_Arrow
  | `Up_Arrow
  | `Num_Lock
  | `Keypad_Slash
  | `Keypad_Asterisk
  | `Keypad_Minus
  | `Keypad_Plus
  | `Keypad_Enter
  | `Keypad_1_End
  | `Keypad_2_Down
  | `Keypad_3_PageDn
  | `Keypad_4_Left
  | `Keypad_5
  | `Keypad_6_Right
  | `Keypad_7_Home
  | `Keypad_8_Up
  | `Keypad_9_PageUp
  | `Keypad_0_Insert
  | `Keypad_dot_Delete
  | `Europe_2
  | `App
  | `Keyboard_Power
  | `Keypad_Equals
  | `F13
  | `F14
  | `F15
  | `F16
  | `F17
  | `F18
  | `F19
  | `F20
  | `F21
  | `F22
  | `F23
  | `F24
  | `Keyboard_Execute
  | `Keyboard_Help
  | `Keyboard_Menu
  | `Keyboard_Select
  | `Keyboard_Stop
  | `Keyboard_Again
  | `Keyboard_Undo
  | `Keyboard_Cut
  | `Keyboard_Copy
  | `Keyboard_Paste
  | `Keyboard_Find
  | `Keyboard_Mute
  | `Keyboard_Volume_Up
  | `Keyboard_Volume_Dn
  | `Keyboard_Locking_Caps_Lock
  | `Keyboard_Locking_Num_Lock
  | `Keyboard_Locking_Scroll_Lock
  | `Keypad_Comma
  | `Keyboard_Equal_Sign
  | `Keyboard_Int'l_6_'PC9800_Keypad_comma_'
  | `Keyboard_Int'l_7
  | `Keyboard_Int'l_8
  | `Keyboard_Int'l_9
  | `Keyboard_Lang_6
  | `Keyboard_Lang_7
  | `Keyboard_Lang_8
  | `Keyboard_Lang_9
  | `Keyboard_Alternate_Erase
  | `Keyboard_SysReq
  | `Keyboard_Cancel
  | `Keyboard_Clear
  | `Keyboard_Prior
  | `Keyboard_Return
  | `Keyboard_Separator
  | `Keyboard_Out
  | `Keyboard_Oper
  | `Keyboard_Clear_Again
  | `Keyboard_CrSel
  | `Keyboard_ExSel
  | `Left_Control
  | `Left_Shift
  | `Left_Alt
  | `Left_Meta
  | `Right_Control
  | `Right_Shift
  | `Right_Alt
  | `Right_Meta
  | `Scan_Next_Track
  | `Scan_Previous_Track
  | `Stop
  | `Play_slash__Pause
  | `Mute
  | `Bass_Boost
  | `Loudness
  | `Volume_Up
  | `Volume_Down
  | `Bass_Up
  | `Bass_Down
  | `Treble_Up
  | `Treble_Down
  | `Media_Select
  | `Mail
  | `Calculator
  | `My_Computer
  | `WWW_Search
  | `WWW_Home
  | `WWW_Back
  | `WWW_Forward
  | `WWW_Stop
  | `WWW_Refresh
  | `WWW_Favorites
  (*| `Lenovo_Fn
       (** Do not depend on this, doesn't work on SDL, see keycodes.ml*)
  *)
]

val pp_keysym : Format.formatter -> keysym -> unit

val find_keysym : ((int * int option * keysym) -> bool) -> keysym option
(** Try to look up the keysym in a
    [USB_HID_CONSTANT * PS2_CONSTANT option * keysym] list*)


type kmod = None | Shift | Caps_lock | Ctrl | Alt | Mod1

val pp_kmod : Format.formatter -> kmod -> unit

val kmods_of_mask : (int -> kmod) -> int -> kmod list
(** [kmods_of_mask FB.kmod_of_constant mask] is a deduplicated list of [kmod]s
    set in the [mask] (an empty list if none). *)


(** Handling keymaps, roughly based on
    {{https://wiki.archlinux.org/index.php/Xmodmap}} *)

module type Keymap = sig
  val to_unicode : kmod list -> keysym -> Uchar.t list
  (** [to_unicode modifiers symbol] returns a list of unicode characters
      corresponding to the [symbol] in the Keymap, an empty list if none. *)
end

module US_keyboard : Keymap
