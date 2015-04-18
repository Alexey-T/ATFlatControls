Controls for Lazarus 

ATSimpleButton
--------------
Btn, not OS themed, it paints as rectangle with 1pix frame. Btns have "pressed", "mouseover", "checked" styles. if Checkable is true, then click toggles Checked, so buttons have "checked" style. Colors of bg and 1pix frame - set them in record, global var "ATButtonTheme".

ATListbox
---------
Listbox, Sublime style, it's not OS themed, not needed Items: set ItemHeight, ItemCount, and OnDrawItem to paint all items (use ItemIndex to hilite). Not focusable, handle keys in Form.OnKeyDown, set ItemIndex.


Lazarus 1.2.6,
Win/Linux
