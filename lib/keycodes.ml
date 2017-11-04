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
  (* | `Lenovo_Fn (* dec: 151 (143) X11 KeySym 0x1008FF2B (XF86WakeUp),
                  this is not recognized by SDL - do not depend on it*)
                 (*| 0x8F or 0x97, `Lenovo_Fn  Set/1: e0 63 / e0 e3  *)
  *)
]

let usbhid_ps2_table : (int * int option * keysym) list = [
0x81 , Some 0xE0_5E , `System_Power ; (* 1 , System Power *)
0x82 , Some 0xE0_5F , `System_Sleep ; (* 1 , System Sleep *)
0x83 , Some 0xE0_63 , `System_Wake ; (* 1 , System Wake *)
0x0 , None , `No_Event ; (* 7 , No Event *)
0x1 , Some 0xFF , `Overrun_Error ; (* 7 , Overrun Error *)
0x2 , Some 0xFC , `POST_Fail ; (* 7 , POST Fail *)
0x3 , None , `ErrorUndefined ; (* 7 , ErrorUndefined *)
0x4 , Some 0x1E , `A ; (* 7 , a A *)
0x5 , Some 0x30 , `B ; (* 7 , b B *)
0x6 , Some 0x2E , `C ; (* 7 , c C *)
0x7 , Some 0x20 , `D ; (* 7 , d D *)
0x8 , Some 0x12 , `E ; (* 7 , e E *)
0x9 , Some 0x21 , `F ; (* 7 , f F *)
0xA , Some 0x22 , `G ; (* 7 , g G *)
0xB , Some 0x23 , `H ; (* 7 , h H *)
0xC , Some 0x17 , `I ; (* 7 , i I *)
0xD , Some 0x24 , `J ; (* 7 , j J *)
0xE , Some 0x25 , `K ; (* 7 , k K *)
0xF , Some 0x26 , `L ; (* 7 , l L *)
0x10 , Some 0x32 , `M ; (* 7 , m M *)
0x11 , Some 0x31 , `N ; (* 7 , n N *)
0x12 , Some 0x18 , `O ; (* 7 , o O *)
0x13 , Some 0x19 , `P ; (* 7 , p P *)
0x14 , Some 0x10 , `Q ; (* 7 , q Q *)
0x15 , Some 0x13 , `R ; (* 7 , r R *)
0x16 , Some 0x1F , `S ; (* 7 , s S *)
0x17 , Some 0x14 , `T ; (* 7 , t T *)
0x18 , Some 0x16 , `U ; (* 7 , u U *)
0x19 , Some 0x2F , `V ; (* 7 , v V *)
0x1A , Some 0x11 , `W ; (* 7 , w W *)
0x1B , Some 0x2D , `X ; (* 7 , x X *)
0x1C , Some 0x15 , `Y ; (* 7 , y Y *)
0x1D , Some 0x2C , `Z ; (* 7 , z Z *)
0x1E , Some 0x02 , `_1 ; (* 7 , 1 ! *)
0x1F , Some 0x03 , `_2 ; (* 7 , 2 @ *)
0x20 , Some 0x04 , `_3 ; (* 7 , 3 # *)
0x21 , Some 0x05 , `_4 ; (* 7 , 4 $ *)
0x22 , Some 0x06 , `_5 ; (* 7 , 5 % *)
0x23 , Some 0x07 , `_6 ; (* 7 , 6 ^ *)
0x24 , Some 0x08 , `_7 ; (* 7 , 7 & *)
0x25 , Some 0x09 , `_8 ; (* 7 , 8 * *)
0x26 , Some 0x0A , `_9 ; (* 7 , 9 ( *)
0x27 , Some 0x0B , `_0 ; (* 7 , 0 ) *)
0x28 , Some 0x1C , `Return ; (* 7 , Return *)
0x29 , Some 0x01 , `Escape ; (* 7 , Escape *)
0x2A , Some 0x0E , `Backspace ; (* 7 , Backspace *)
0x2B , Some 0x0F , `Tab ; (* 7 , Tab / ISO_Left_tab *)
0x2C , Some 0x39 , `Space ; (* 7 , Space *)
0x2D , Some 0x0C , `Minus ; (* 7 , - _ *)
0x2E , Some 0x0D , `Equals ; (* 7 , = + *)
0x2F , Some 0x1A , `Leftbracket ; (* 7 , [ { *)
0x30 , Some 0x1B , `Rightbracket ; (* 7 , ] } *)
0x31 , Some 0x2B , `Backslash ; (* 7 , \ | *)
0x32 , None , `Europe_1 ; (* 7 , Europe 1*)
0x33 , Some 0x27 , `Semicolon ; (* 7 , ; : *)
0x34 , Some 0x28 , `Apostrophe ; (* 7 , '' "" *)
0x35 , Some 0x29 , `Grave ; (* 7 , ` ~ *)
0x36 , Some 0x33 , `Comma ; (* 7 , , < *)
0x37 , Some 0x34 , `Period ; (* 7 , . > *)
0x38 , Some 0x35 , `Slash ; (* 7 , / ? *)
0x39 , Some 0x3A , `Caps_Lock ; (* 7 , Caps Lock *)
0x3A , Some 0x3B , `F1 ; (* 7 , F1 *)
0x3B , Some 0x3C , `F2 ; (* 7 , F2 *)
0x3C , Some 0x3D , `F3 ; (* 7 , F3 *)
0x3D , Some 0x3E , `F4 ; (* 7 , F4 *)
0x3E , Some 0x3F , `F5 ; (* 7 , F5 *)
0x3F , Some 0x40 , `F6 ; (* 7 , F6 *)
0x40 , Some 0x41 , `F7 ; (* 7 , F7 *)
0x41 , Some 0x42 , `F8 ; (* 7 , F8 *)
0x42 , Some 0x43 , `F9 ; (* 7 , F9 *)
0x43 , Some 0x44 , `F10 ; (* 7 , F10 *)
0x44 , Some 0x57 , `F11 ; (* 7 , F11 *)
0x45 , Some 0x58 , `F12 ; (* 7 , F12 *)
0x46 , Some 0xE0_37 , `Print_Screen ; (* 7 , Print *)
0x47 , Some 0x46 , `Scroll_Lock ; (* 7 , Scroll Lock *)
0x48 , Some 0xE0_46_E0_C6 , `Break ; (* 7 , Break (Ctrl-Pause) *)
0x48 , Some 0xE1_1D_45_E1_9D_C5 , `Pause ; (* 7 , Pause *)
0x49 , Some 0xE0_52 , `Insert ; (* 7 , Insert  *) (* x11 dec: 118 *)
0x4A , Some 0xE0_47 , `Home ; (* 7 , Home  *) (* x11 dec: 110 *)
0x4B , Some 0xE0_49 , `Page_Up ; (* 7 , Page Up  *)
0x4C , Some 0xE0_53 , `Delete ; (* 7 , Delete  *)
0x4D , Some 0xE0_4F , `End ; (* 7 , End  *) (* x11 dec: 115 *)
0x4E , Some 0xE0_51 , `Page_Down ; (* 7 , Page Down  *) (* x11 dec: 117 *)
0x4F , Some 0xE0_4D , `Right_Arrow ; (* 7 , Right Arrow  *) (* x11 dec: 114*)
0x50 , Some 0xE0_4B , `Left_Arrow ; (* 7 , Left Arrow  *) (* x11 dec: 113 *)
0x51 , Some 0xE0_50 , `Down_Arrow ; (* 7 , Down Arrow  *) (* x11 dec: 116 *)
0x52 , Some 0xE0_48 , `Up_Arrow ; (* 7 , Up Arrow  *)
0x53 , Some 0x45 , `Num_Lock ; (* 7 , Num Lock *)
0x54 , Some 0xE0_35 , `Keypad_Slash ; (* 7 , Keypad / *)(* x11: 0x62 KP_Divide*)
0x55 , Some 0x37 , `Keypad_Asterisk ; (* 7 , Keypad * *)
0x56 , Some 0x4A , `Keypad_Minus ; (* 7 , Keypad - *)
0x57 , Some 0x4E , `Keypad_Plus ; (* 7 , Keypad + *)
0x58 , Some 0xE0_1C , `Keypad_Enter ; (* 7 , Keypad Enter - x11: 0x60*)
0x59 , Some 0x4F , `Keypad_1_End ; (* 7 , Keypad 1 End *)
0x5A , Some 0x50 , `Keypad_2_Down ; (* 7 , Keypad 2 Down *)
0x5B , Some 0x51 , `Keypad_3_PageDn ; (* 7 , Keypad 3 PageDn aka "KP_Next" *)
0x5C , Some 0x4B , `Keypad_4_Left ; (* 7 , Keypad 4 Left *)
0x5D , Some 0x4C , `Keypad_5 ; (* 7 , Keypad 5 *)
0x5E , Some 0x4D , `Keypad_6_Right ; (* 7 , Keypad 6 Right *)
0x5F , Some 0x47 , `Keypad_7_Home ; (* 7 , Keypad 7 Home *)
0x60 , Some 0x48 , `Keypad_8_Up ; (* 7 , Keypad 8 Up *)
0x61 , Some 0x49 , `Keypad_9_PageUp ; (* 7 , Keypad 9 PageUp aka "KP_Prior" *)
0x62 , Some 0x52 , `Keypad_0_Insert ; (* 7 , Keypad 0 Insert *)
0x63 , Some 0x53 , `Keypad_dot_Delete ; (* 7 , Keypad . Delete *)
0x64 , Some 0x56 , `Europe_2 ; (* 7 , Europe 2, aka <> \ | *)
0x65 , Some 0xE0_5D , `App ; (* 7 , App *)
0x66 , None , `Keyboard_Power ; (* 7 , Keyboard Power *)
0x67 , Some 0x59 , `Keypad_Equals ; (* 7 , Keypad = *)
0x68 , Some 0x5D , `F13 ; (* 7 , F13 *)
0x69 , Some 0x5E , `F14 ; (* 7 , F14 *)
0x6A , Some 0x5F , `F15 ; (* 7 , F15 *)
0x6B , None , `F16 ; (* 7 , F16 *)
0x6C , None , `F17 ; (* 7 , F17 *)
0x6D , None , `F18 ; (* 7 , F18 *)
0x6E , None , `F19 ; (* 7 , F19 *)
0x6F , None , `F20 ; (* 7 , F20 *)
0x70 , None , `F21 ; (* 7 , F21 *)
0x71 , None , `F22 ; (* 7 , F22 *)
0x72 , None , `F23 ; (* 7 , F23 *)
0x73 , None , `F24 ; (* 7 , F24 *)
0x74 , None , `Keyboard_Execute ; (* 7 , Keyboard Execute *)
0x75 , None , `Keyboard_Help ; (* 7 , Keyboard Help *)
0x76 , None , `Keyboard_Menu ; (* 7 , Keyboard Menu *)
0x77 , None , `Keyboard_Select ; (* 7 , Keyboard Select *)
0x78 , None , `Keyboard_Stop ; (* 7 , Keyboard Stop *)
0x79 , None , `Keyboard_Again ; (* 7 , Keyboard Again *)
0x7A , None , `Keyboard_Undo ; (* 7 , Keyboard Undo *)
0x7B , None , `Keyboard_Cut ; (* 7 , Keyboard Cut *)
0x7C , None , `Keyboard_Copy ; (* 7 , Keyboard Copy *)
0x7D , None , `Keyboard_Paste ; (* 7 , Keyboard Paste *)
0x7E , None , `Keyboard_Find ; (* 7 , Keyboard Find *)
0x7F , None , `Keyboard_Mute ; (* 7 , Keyboard Mute *)
0x80 , None , `Keyboard_Volume_Up ; (* 7 , Keyboard Volume Up *)
0x81 , None , `Keyboard_Volume_Dn ; (* 7 , Keyboard Volume Dn *)
0x82 , None , `Keyboard_Locking_Caps_Lock ; (* 7 , Keyboard Locking Caps Lock *)
0x83 , None , `Keyboard_Locking_Num_Lock ; (* 7 , Keyboard Locking Num Lock *)
0x84 , None , `Keyboard_Locking_Scroll_Lock ; (* 7 , Keyboard Locking Scroll Lock *)
0x85 , Some 0x7E , `Keypad_Comma ; (* 7 , Keypad , (Brazilian Keypad .) *)
0x86 , None , `Keyboard_Equal_Sign ; (* 7 , Keyboard Equal Sign *)
0x8C , Some 0x5C , `Keyboard_Int'l_6_'PC9800_Keypad_comma_' ; (* 7 , Keyboard Int'l 6 (PC9800 Keypad , ) *)
0x8D , None , `Keyboard_Int'l_7 ; (* 7 , Keyboard Int'l 7 *)
0x8E , None , `Keyboard_Int'l_8 ; (* 7 , Keyboard Int'l 8 *)
0x8F , None , `Keyboard_Int'l_9 ; (* 7 , Keyboard Int'l 9 *)
0x95 , None , `Keyboard_Lang_6 ; (* 7 , Keyboard Lang 6 *)
0x96 , None , `Keyboard_Lang_7 ; (* 7 , Keyboard Lang 7 *)
0x97 , None , `Keyboard_Lang_8 ; (* 7 , Keyboard Lang 8 *)
0x98 , None , `Keyboard_Lang_9 ; (* 7 , Keyboard Lang 9 *)
0x99 , None , `Keyboard_Alternate_Erase ; (* 7 , Keyboard Alternate Erase *)
0x9A , None , `Keyboard_SysReq ; (* 7 , Keyboard SysReq/Attention *)
0x9B , None , `Keyboard_Cancel ; (* 7 , Keyboard Cancel *)
0x9C , None , `Keyboard_Clear ; (* 7 , Keyboard Clear *)
0x9D , None , `Keyboard_Prior ; (* 7 , Keyboard Prior *)
0x9E , None , `Keyboard_Return ; (* 7 , Keyboard Return *)
0x9F , None , `Keyboard_Separator ; (* 7 , Keyboard Separator *)
0xA0 , None , `Keyboard_Out ; (* 7 , Keyboard Out *)
0xA1 , None , `Keyboard_Oper ; (* 7 , Keyboard Oper *)
0xA2 , None , `Keyboard_Clear_Again ; (* 7 , Keyboard Clear/Again *)
0xA3 , None , `Keyboard_CrSel ; (* 7 , Keyboard CrSel/Props *)
0xA4 , None , `Keyboard_ExSel ; (* 7 , Keyboard ExSel *)
0xE0 , Some 0x1D , `Left_Control ; (* 7 , Left Control *)
0xE1 , Some 0x2A , `Left_Shift ; (* 7 , Left Shift *)
0xE2 , Some 0x38 , `Left_Alt ; (* 7 , Left Alt *)
0xE3 , Some 0xE0_5B , `Left_Meta ; (* 7 , Left Meta *)
0xE4 , Some 0xE0_1D , `Right_Control ; (* 7 , Right Control *)
0xE5 , Some 0x36 , `Right_Shift ; (* 7 , Right Shift *)
0xE6 , Some 0xE0_38 , `Right_Alt ; (* 7 , Right Alt *)
0xE7 , Some 0xE0_5C , `Right_Meta ; (* 7 , Right Meta *)
0xB5 , Some 0xE0_19 , `Scan_Next_Track ; (* 12 , Scan Next Track *)
0xB6 , Some 0xE0_10 , `Scan_Previous_Track ; (* 12 , Scan Previous Track *)
0xB7 , Some 0xE0_24 , `Stop ; (* 12 , Stop *)
0xCD , Some 0xE0_22 , `Play_slash__Pause ; (* 12 , Play/ Pause *)
0xE2 , Some 0xE0_20 , `Mute ; (* 12 , Mute *)
0xE5 , None , `Bass_Boost ; (* 12 , Bass Boost *)
0xE7 , None , `Loudness ; (* 12 , Loudness *)
0xE9 , Some 0xE0_30 , `Volume_Up ; (* 12 , Volume Up *)
0xEA , Some 0xE0_2E , `Volume_Down ; (* 12 , Volume Down *)
0x152 , None , `Bass_Up ; (* 12 , Bass Up *)
0x153 , None , `Bass_Down ; (* 12 , Bass Down *)
0x154 , None , `Treble_Up ; (* 12 , Treble Up *)
0x155 , None , `Treble_Down ; (* 12 , Treble Down *)
0x183 , Some 0xE0_6D , `Media_Select ; (* 12 , Media Select *)
0x18A , Some 0xE0_6C , `Mail ; (* 12 , Mail *)
0x192 , Some 0xE0_21 , `Calculator ; (* 12 , Calculator *)
0x194 , Some 0xE0_6B , `My_Computer ; (* 12 , My Computer *)
0x221 , Some 0xE0_65 , `WWW_Search ; (* 12 , WWW Search *)
0x223 , Some 0xE0_32 , `WWW_Home ; (* 12 , WWW Home *)
0x224 , Some 0xE0_6A , `WWW_Back ; (* 12 , WWW Back *)
0x225 , Some 0xE0_69 , `WWW_Forward ; (* 12 , WWW Forward *)
0x226 , Some 0xE0_68 , `WWW_Stop ; (* 12 , WWW Stop *)
0x227 , Some 0xE0_67 , `WWW_Refresh ; (* 12 , WWW Refresh *)
0x22A , Some 0xE0_66 , `WWW_Favorites ; (* 12 , WWW Favorites *)
(*None, Some 0x54, `ISO_Level3_Shift *)
]

let pp_keysym (fmt:Format.formatter) (key:keysym) : unit =
  Fmt.pf fmt "%s" @@ begin match key with
  | `System_Power -> "System Power"
  | `System_Sleep -> "System Sleep"
  | `System_Wake -> "System Wake"
  | `No_Event -> "No Event"
  | `Overrun_Error -> "Overrun Error"
  | `POST_Fail -> "POST Fail"
  | `ErrorUndefined -> "ErrorUndefined"
  | `A -> "a A"
  | `B -> "b B"
  | `C -> "c C"
  | `D -> "d D"
  | `E -> "e E"
  | `F -> "f F"
  | `G -> "g G"
  | `H -> "h H"
  | `I -> "i I"
  | `J -> "j J"
  | `K -> "k K"
  | `L -> "l L"
  | `M -> "m M"
  | `N -> "n N"
  | `O -> "o O"
  | `P -> "p P"
  | `Q -> "q Q"
  | `R -> "r R"
  | `S -> "s S"
  | `T -> "t T"
  | `U -> "u U"
  | `V -> "v V"
  | `W -> "w W"
  | `X -> "x X"
  | `Y -> "y Y"
  | `Z -> "z Z"
  | `_1 -> "1 !"
  | `_2 -> "2 @"
  | `_3 -> "3 #"
  | `_4 -> "4 $"
  | `_5 -> "5 %"
  | `_6 -> "6 ^"
  | `_7 -> "7 &"
  | `_8 -> "8 *"
  | `_9 -> "9 ("
  | `_0 -> "0 )"
  | `Return -> "Return"
  | `Escape -> "Escape"
  | `Backspace -> "Backspace"
  | `Tab -> "Tab"
  | `ISO_Left_Tab -> "ISO_Left_Tab"
  | `Space -> "Space"
  | `Minus -> "- _"
  | `Equals -> "= +"
  | `Leftbracket -> "[ {"
  | `Rightbracket -> "] }"
  | `Backslash -> "\ |"
  | `Europe_1 -> "Europe 1"
  | `Semicolon -> "; :"
  | `Apostrophe -> "' \""
  | `Grave -> "` ~"
  | `Comma -> ", <"
  | `Period -> ". >"
  | `Slash -> "/ ?"
  | `Caps_Lock -> "Caps Lock"
  | `F1 -> "F1"
  | `F2 -> "F2"
  | `F3 -> "F3"
  | `F4 -> "F4"
  | `F5 -> "F5"
  | `F6 -> "F6"
  | `F7 -> "F7"
  | `F8 -> "F8"
  | `F9 -> "F9"
  | `F10 -> "F10"
  | `F11 -> "F11"
  | `F12 -> "F12"
  | `Print_Screen -> "Print Screen "
  | `Scroll_Lock -> "Scroll Lock"
  | `Break -> "Break (Ctrl-Pause)"
  | `Pause -> "Pause"
  | `Insert -> "Insert "
  | `Home -> "Home "
  | `Page_Up -> "Page Up "
  | `Delete -> "Delete "
  | `End -> "End "
  | `Page_Down -> "Page Down "
  | `Right_Arrow -> "Right Arrow "
  | `Left_Arrow -> "Left Arrow "
  | `Down_Arrow -> "Down Arrow "
  | `Up_Arrow -> "Up Arrow "
  | `Num_Lock -> "Num Lock"
  | `Keypad_Slash -> "Keypad /"
  | `Keypad_Asterisk -> "Keypad *"
  | `Keypad_Minus -> "Keypad -"
  | `Keypad_Plus -> "Keypad +"
  | `Keypad_Enter -> "Keypad Enter"
  | `Keypad_1_End -> "Keypad 1 End"
  | `Keypad_2_Down -> "Keypad 2 Down"
  | `Keypad_3_PageDn -> "Keypad 3 PageDn"
  | `Keypad_4_Left -> "Keypad 4 Left"
  | `Keypad_5 -> "Keypad 5"
  | `Keypad_6_Right -> "Keypad 6 Right"
  | `Keypad_7_Home -> "Keypad 7 Home"
  | `Keypad_8_Up -> "Keypad 8 Up"
  | `Keypad_9_PageUp -> "Keypad 9 PageUp"
  | `Keypad_0_Insert -> "Keypad 0 Insert"
  | `Keypad_dot_Delete -> "Keypad . Delete"
  | `Europe_2 -> "Europe 2"
  | `App -> "App"
  | `Keyboard_Power -> "Keyboard Power"
  | `Keypad_Equals -> "Keypad ="
  | `F13 -> "F13"
  | `F14 -> "F14"
  | `F15 -> "F15"
  | `F16 -> "F16"
  | `F17 -> "F17"
  | `F18 -> "F18"
  | `F19 -> "F19"
  | `F20 -> "F20"
  | `F21 -> "F21"
  | `F22 -> "F22"
  | `F23 -> "F23"
  | `F24 -> "F24"
  | `Keyboard_Execute -> "Keyboard Execute"
  | `Keyboard_Help -> "Keyboard Help"
  | `Keyboard_Menu -> "Keyboard Menu"
  | `Keyboard_Select -> "Keyboard Select"
  | `Keyboard_Stop -> "Keyboard Stop"
  | `Keyboard_Again -> "Keyboard Again"
  | `Keyboard_Undo -> "Keyboard Undo"
  | `Keyboard_Cut -> "Keyboard Cut"
  | `Keyboard_Copy -> "Keyboard Copy"
  | `Keyboard_Paste -> "Keyboard Paste"
  | `Keyboard_Find -> "Keyboard Find"
  | `Keyboard_Mute -> "Keyboard Mute"
  | `Keyboard_Volume_Up -> "Keyboard Volume Up"
  | `Keyboard_Volume_Dn -> "Keyboard Volume Dn"
  | `Keyboard_Locking_Caps_Lock -> "Keyboard Locking Caps Lock"
  | `Keyboard_Locking_Num_Lock -> "Keyboard Locking Num Lock"
  | `Keyboard_Locking_Scroll_Lock -> "Keyboard Locking Scroll Lock"
  | `Keypad_Comma -> "Keypad , (Brazilian Keypad .)"
  | `Keyboard_Equal_Sign -> "Keyboard Equal Sign"
  | `Keyboard_Int'l_6_'PC9800_Keypad_comma_' -> "Keyboard Int'l 6 (PC9800 Keypad , )"
  | `Keyboard_Int'l_7 -> "Keyboard Int'l 7"
  | `Keyboard_Int'l_8 -> "Keyboard Int'l 8"
  | `Keyboard_Int'l_9 -> "Keyboard Int'l 9"
  | `Keyboard_Lang_6 -> "Keyboard Lang 6"
  | `Keyboard_Lang_7 -> "Keyboard Lang 7"
  | `Keyboard_Lang_8 -> "Keyboard Lang 8"
  | `Keyboard_Lang_9 -> "Keyboard Lang 9"
  | `Keyboard_Alternate_Erase -> "Keyboard Alternate Erase"
  | `Keyboard_SysReq -> "Keyboard SysReq/Attention"
  | `Keyboard_Cancel -> "Keyboard Cancel"
  | `Keyboard_Prior -> "Keyboard Prior"
  | `Keyboard_Return -> "Keyboard Return"
  | `Keyboard_Separator -> "Keyboard Separator"
  | `Keyboard_Out -> "Keyboard Out"
  | `Keyboard_Oper -> "Keyboard Oper"
  | `Keyboard_Clear -> "Keyboard Clear"
  | `Keyboard_Clear_Again -> "Keyboard Clear/Again"
  | `Keyboard_CrSel -> "Keyboard CrSel/Props"
  | `Keyboard_ExSel -> "Keyboard ExSel"
  | `Left_Control -> "Left Control"
  | `Left_Shift -> "Left Shift"
  | `Left_Alt -> "Left Alt"
  | `Left_Meta -> "Left Meta"
  | `Right_Control -> "Right Control"
  | `Right_Shift -> "Right Shift"
  | `Right_Alt -> "Right Alt"
  | `Right_Meta -> "Right Meta"
  | `Scan_Next_Track -> "Scan Next Track"
  | `Scan_Previous_Track -> "Scan Previous Track"
  | `Stop -> "Stop"
  | `Play_slash__Pause -> "Play/ Pause"
  | `Mute -> "Mute"
  | `Bass_Boost -> "Bass Boost"
  | `Loudness -> "Loudness"
  | `Volume_Up -> "Volume Up"
  | `Volume_Down -> "Volume Down"
  | `Bass_Up -> "Bass Up"
  | `Bass_Down -> "Bass Down"
  | `Treble_Up -> "Treble Up"
  | `Treble_Down -> "Treble Down"
  | `Media_Select -> "Media Select"
  | `Mail -> "Mail"
  | `Calculator -> "Calculator"
  | `My_Computer -> "My Computer"
  | `WWW_Search -> "WWW Search"
  | `WWW_Home -> "WWW Home"
  | `WWW_Back -> "WWW Back"
  | `WWW_Forward -> "WWW Forward"
  | `WWW_Stop -> "WWW Stop"
  | `WWW_Refresh -> "WWW Refresh"
  | `WWW_Favorites -> "WWW Favorites"
  (*| `Lenovo_Fn -> "Lenovo Fn (or XF86WakeUp)" *)
end (* pp_ *)

let find_keysym f =
  let rec search = function
    | ((_,_,sym) as v)::_ when f v -> Some sym
    | _::tl -> search tl
    | [] -> None
  in search usbhid_ps2_table

type kmod =
    | None
(*    | Shift_L 0x32
    | Shift_R 0x3e *)
    | Shift
    | Caps_lock (* 0x42 *)

    (*    | Ctrl_L 0x25
    | Ctrl_R 0x69 *)
    | Ctrl
(*    | Alt_L 0x40
      | Alt_R 0x6c *)
    | Alt
    | Mod1
      (* | Meta_L 0xcd
         | Super_L 0x85
         | Super_L 0xce
         | Super_R 0x86
         | Hyper_L 0xcf
         | ISO_Level3_Shift 0x5c
         | Mode_switch 0xcb
         | Num_lock 0x4d
       *)

let pp_kmod (fmt:Format.formatter) kmod =
  Fmt.pf fmt "%s" @@ begin match kmod with
    | None -> "None"
    | Shift -> "Shift"
    | Caps_lock -> "Caps_lock"
    | Ctrl -> "Ctrl"
    | Alt -> "Alt"
    | Mod1 -> "Mod1"
  end

let kmods_of_mask lookup mask =
  let rec f acc : int -> kmod list = function
    | 0 -> List.filter (function None -> false | _ -> true) acc
           |> List.sort_uniq compare
    | i ->
      let constant = mask land (1 lsl (i-1)) in
      (i-1)
      |> if constant <> 0
         then f (lookup constant :: acc)
         else f acc
  in
  f [] 14 (* 14: check up to 8192 (Caps_lock)*)


type keymap_modeswitch =
  { n: Uchar.t ; (* none *)
    s: Uchar.t ; (* shift *)
    ms: Uchar.t ; (* modeswitch (alt) *)
    s_ms: Uchar.t ; (* shift + modeswitch (alt) *)
  }

type keymap = (keysym * keymap_modeswitch) list (* xmodmap -pke *)

(* type modifiermap - xmodmap -pm*)
(* type pointermap*)


module type Keymap =
sig
  (*val t : keymap*)
  val to_unicode : kmod list -> keysym -> Uchar.t list
end

(*
let kadd sym none ?shift ?modeswitch ?shift_modeswitch () =
  let s = match shift with None -> none | Some s -> s in
  let ms = match modeswitch with None -> none | Some ms -> ms in
  let s_ms = match shift_modeswitch with None -> s | Some s_ms -> s_ms in
  sym, { n = none; s; ms; s_ms; }
*)

let kshift sym normal shift =
  sym, { n = Uchar.of_int normal ;
         s = Uchar.of_int shift;
         ms = Uchar.of_int normal;
         s_ms = Uchar.of_int shift;}

let default : keymap =
  List.mapi (fun i sym -> kshift sym (i+0x61) (i+0x41))
    [ `A; `B; `C; `D; `E; `F; `G; `H; `I; `J; `K; `L; `M;
      `N; `O; `P; `Q; `R; `S; `T; `U; `V; `W; `X; `Y; `Z;]
  @ List.mapi (fun i sym -> kshift sym (i+0x30)
    @@ Char.code [|')'; '!'; '@'; '#'; '$'; '%'; '^'; '&'; '*'; '('|].(i))
    [ `_0; `_1; `_2; `_3; `_4; `_5; `_6; `_7; `_8; `_9 ]

module US_keyboard : Keymap = struct
  let t : keymap =
    [kshift `_1 0x31 (Char.code '!')
    ] @ default

  let to_unicode modifiers sym : Uchar.t list =
    let select_sym =
      begin match List.mem Shift modifiers with (* TODO handle modeswitch *)
      | true -> (function s -> s.s)
      | false -> (fun s -> s.n)
      end
    in
    begin match List.assoc sym t with
      | exception Not_found -> []
      | m -> [select_sym m]
    end
end
