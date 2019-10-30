OnTabClick
=========

Called when normal tab activates (by click or code).

OnTabPlusClick
=========

Called when "plus" pseudo-tab, or "plus" button (added in OptButtonLayout), clicked.

OnTabClickUserButton
=========

Called when user button clicked, which is char "0".."4" in OptButtonLayout.

OnTabClose
=========

Called when "x" mark clicked. You can override default tab deletion and show confirmation before closing a tab.

OnTabMenu
=========

Called when "down arrow" clicked, which is "v" char in OptButtonLayout. You can disable default dropdown menu show.

OnTabDrawBefore
=========

Called before painting a tab or another element. You can disable default painting. Not called for user buttons.

OnTabDrawAfter
=========

Called after painting a tab or another element. This is called also for user buttons (chars 
"0".."4" in OptButtonLayout).

OnTabEmpty
=========

Called after control has no tabs anymore, after deleting last tab.

OnTabOver
=========

Called when mouse moves over control. Mouse-over tab index is passed to event.

OnTabMove
=========

Called after adding/deleting/moving tabs. Passed 2 indexes: NFrom, NTo. NFrom=-1 for adding, NTo=-1 for 
deleting, both >= 0 for moving.

OnTabChangeQuery
==========

Called before activating new tab. Can disable this activation.
