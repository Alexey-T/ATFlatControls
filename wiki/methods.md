
    function GetTabRectWidth(APlusBtn: boolean): Integer;

Returns width of normal or "plus" tab.

    function GetTabRect(AIndex: Integer): TRect;

Returns coords of usual tabs.

    function GetTabRect_Plus: TRect;

Returns coords of "plus" tab.

    function GetTabRect_X(const ARect: TRect): TRect;

Returns coords of "x" button for given tab coords.

    procedure GetArrowRect(var RLeft, RRight, RDown: TRect);

Returns coords of arrow buttons. Currently only "down" arrow is used (last param).

    function GetTabAt(X, Y: Integer): Integer;

Returns index of tab at given coords. Negative values mean special tabs: "plus", "arrow".

    function GetTabData(AIndex: Integer): TATTabData;

Returns object with tab information. (nil for wrong index).

    function TabCount: Integer;

Number of tabs.

    procedure AddTab(AIndex: Integer;
      const ACaption: string;
      AObject: TObject = nil;
      AModified: boolean = false;
      AColor: TColor = clNone);

Adds new tab. AIndex is index of tab or -1 to append to end. AObject, AModified are not used by control (use these if needed). AColor (if not clNone) sets tab hilite color.

    function DeleteTab(AIndex: Integer; AAllowEvent, AWithCancelBtn: boolean): boolean;

Deletes tab (passed AObject is not freed).
`AAllowEvent` allows to call `OnTabClose` after this.
`AWithCancelBtn` specifies, what value will be passed to `OnTabClose` parameter `ACanContinue` (if you delete only one tab, pass False, if you're in a loop which deletes many tabs at once, pass True).

    procedure ShowTabMenu;

Shows tabs menu (at position of "down arrow" button).

    procedure SwitchTab(ANext: boolean);

Activates next or previous tab. (After last one the first activates).
