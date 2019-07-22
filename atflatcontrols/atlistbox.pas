{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATListbox;

{$ifdef FPC}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  Forms, {$ifndef FPC}Messages, Windows,{$endif}
  {$ifdef FPC}
  LMessages,
  {$endif}
  ATScrollBar,
  ATFlatThemes;

type
  TATListboxDrawItemEvent = procedure(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect) of object;

type
  TATIntArray = array of integer;

type
  { TATListbox }

  TATListbox = class(TCustomControl)
  private
    FTheme: PATFlatTheme;
    FThemedScrollbar: boolean;
    FScrollbar: TATScrollbar;
    FOwnerDrawn: boolean;
    FVirtualMode: boolean;
    FVirtualItemCount: integer;
    FItemIndex: integer;
    FItemHeightPercents: integer;
    FItemHeight: integer;
    FItemHeightIsFixed: boolean;
    FItemTop: integer;
    FBitmap: Graphics.TBitmap;
    FCanGetFocus: boolean;
    FList: TStringList;
    FHotTrack: boolean;
    FHotTrackIndex: integer;
    FIndentLeft: integer;
    FIndentTop: integer;
    FColumnSep: char;
    FColumnSizes: TATIntArray;
    FColumnWidths: TATIntArray;
    FOnDrawItem: TATListboxDrawItemEvent;
    FOnChangeSel: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    procedure DoDefaultDrawItem(C: TCanvas; AIndex: integer; R: TRect);
    procedure DoPaintTo(C: TCanvas; r: TRect);
    function ItemBottom: integer;
    procedure ScrollbarChange(Sender: TObject);
    procedure SetCanBeFocused(AValue: boolean);
    procedure SetItemHeightPercents(AValue: integer);
    procedure SetVirtualItemCount(AValue: integer);
    procedure SetItemIndex(AValue: integer);
    procedure SetItemTop(AValue: integer);
    procedure SetItemHeight(AValue: integer);
    procedure SetThemedScrollbar(AValue: boolean);
    procedure UpdateColumnWidths;
    {$ifdef FPC}
    procedure UpdateFromScrollbarMsg(const Msg: TLMScroll);
    {$endif}
    {$ifndef FPC}
    procedure UpdateFromScrollbarMsg(const Msg: TWMVScroll);
    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
    procedure UpdateScrollbar;
    function GetVisibleItems: integer;
    function GetItemHeightDefault: integer;
    function GetColumnWidth(AIndex: integer): integer;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState);
    procedure InvalidateNoSB;
  protected
    procedure Paint; override;
    procedure Click; override;
    {$ifdef FPC}
    procedure LMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
    {$endif}
    {$ifndef FPC}
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    {$endif}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure ChangedSelection; virtual;
    procedure Scrolled; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items: TStringList read FList;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ItemTop: integer read FItemTop write SetItemTop;
    property ItemHeight: integer read FItemHeight write SetItemHeight;
    property ItemHeightDefault: integer read GetItemHeightDefault;
    function ItemCount: integer;
    function IsIndexValid(AValue: integer): boolean;
    property HotTrackIndex: integer read FHotTrackIndex;
    property VirtualItemCount: integer read FVirtualItemCount write SetVirtualItemCount;
    property VisibleItems: integer read GetVisibleItems;
    function GetItemIndexAt(Pnt: TPoint): integer;
    property Theme: PATFlatTheme read FTheme write FTheme;
    property ThemedScrollbar: boolean read FThemedScrollbar write SetThemedScrollbar;
    property Scrollbar: TATScrollbar read FScrollbar;
    property ColumnSeparator: char read FColumnSep write FColumnSep;
    property ColumnSizes: TATIntArray read FColumnSizes write FColumnSizes;
    property ColumnWidth[AIndex: integer]: integer read GetColumnWidth;
    {$ifdef FPC}
    function CanFocus: boolean; override;
    function CanSetFocus: boolean; override;
    {$endif}
    function ClientWidth: integer;
    procedure Invalidate; override;
    procedure UpdateItemHeight;
  published
    property Align;
    property Anchors;
    {$ifdef FPC}
    property BorderStyle;
    property BorderSpacing;
    {$endif}
    property CanGetFocus: boolean read FCanGetFocus write SetCanBeFocused default false;
    property DoubleBuffered stored false;
    property Enabled;
    property HotTrack: boolean read FHotTrack write FHotTrack default false;
    property IndentLeft: integer read FIndentLeft write FIndentLeft default 4;
    property IndentTop: integer read FIndentTop write FIndentTop default 2;
    property ItemHeightPercents: integer read FItemHeightPercents write SetItemHeightPercents default 100;
    property OwnerDrawn: boolean read FOwnerDrawn write FOwnerDrawn default false;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property VirtualMode: boolean read FVirtualMode write FVirtualMode default true;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnContextPopup;
    property OnChangedSel: TNotifyEvent read FOnChangeSel write FOnChangeSel;
    property OnDrawItem: TATListboxDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnResize;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;


implementation

uses
  Math
  {$ifdef FPC}
  , Types,
  InterfaceBase,
  LCLType, LCLIntf
  {$endif};

function SGetItem(var S: string; const ch: Char): string;
var
  i: integer;
begin
  i:= Pos(ch, S);
  if i=0 then
  begin
    Result:= S;
    S:= '';
  end
  else
  begin
    Result:= Copy(S, 1, i-1);
    Delete(S, 1, i);
  end;
end;

function IsDoubleBufferedNeeded: boolean;
begin
  Result := true;
  {$ifdef FPC}
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
  {$endif}
end;

{ TATListbox }

function TATListbox.GetVisibleItems: integer;
begin
  Result:= ClientHeight div FItemHeight;
end;

function TATListbox.IsIndexValid(AValue: integer): boolean;
begin
  Result:= (AValue>=0) and (AValue<ItemCount);
end;

function TATListbox.GetItemHeightDefault: integer;
begin
  Result:= FTheme^.DoScaleFont(FTheme^.FontSize) * 18 div 10 + 2;
  Result:= Result * Screen.PixelsPerInch div 96;
end;

procedure TATListbox.UpdateItemHeight;
begin
  if not FItemHeightIsFixed then
    FItemHeight:= GetItemHeightDefault * FItemHeightPercents div 100;
end;

procedure TATListbox.ChangedSelection;
begin
  if Assigned(FOnChangeSel) then
    FOnChangeSel(Self);
end;

procedure TATListbox.Scrolled;
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TATListbox.UpdateScrollbar;
var
  si: TScrollInfo;
begin
  if ThemedScrollbar then
  begin
    FScrollbar.Min:= 0;
    FScrollbar.Max:= ItemCount;
    FScrollbar.PageSize:= VisibleItems;
    FScrollbar.Position:= ItemTop;
  end;

  FillChar(si{%H-}, SizeOf(si), 0);
  si.cbSize:= SizeOf(si);
  si.fMask:= SIF_ALL;
  si.nMin:= 0;

  if ThemedScrollbar then
  begin
    si.nMax:= 1;
    si.nPage:= 2;
    si.nPos:= 0;
  end
  else
  begin
    si.nMax:= ItemCount;
    si.nPage:= GetVisibleItems;
    si.nPos:= FItemTop;
  end;

  SetScrollInfo(Handle, SB_VERT, si, True);
end;

function TATListbox.ItemCount: integer;
begin
  if FVirtualMode then
    Result:= FVirtualItemCount
  else
    Result:= Items.Count;
end;


procedure TATListbox.DoPaintTo(C: TCanvas; r: TRect);
var
  Index: integer;
begin
  C.Brush.Color:= ColorToRGB(FTheme^.ColorBgListbox);
  C.FillRect(r);

  for Index:= FItemTop to ItemCount-1 do
  begin
    r.Top:= (Index-FItemTop)*FItemHeight;
    r.Bottom:= r.Top+FItemHeight;
    r.Left:= 0;
    r.Right:= ClientWidth;
    if r.Top>=ClientHeight then Break;

    if FOwnerDrawn then
    begin
      if Assigned(FOnDrawItem) then
        FOnDrawItem(Self, C, Index, r);
    end
    else
      DoDefaultDrawItem(C, Index, r);
  end;
end;

function TATListbox.GetColumnWidth(AIndex: integer): integer;
begin
  if (AIndex>=0) and (AIndex<Length(FColumnSizes)) then
    Result:= FColumnWidths[AIndex]
  else
    Result:= 0;
end;

procedure TATListbox.UpdateColumnWidths;
var
  NTotalWidth, NAutoSized, NSize, NFixedSize, i: integer;
begin
  NTotalWidth:= ClientWidth;
  NAutoSized:= 0;
  NFixedSize:= 0;

  SetLength(FColumnWidths, Length(FColumnSizes));

  //set width of fixed columns
  for i:= 0 to Length(FColumnSizes)-1 do
  begin
    NSize:= FColumnSizes[i];

    //auto-sized?
    if NSize=0 then
      Inc(NAutoSized)
    else
    //in percents?
    if NSize<0 then
      NSize:= NTotalWidth * -NSize div 100;

    Inc(NFixedSize, NSize);
    FColumnWidths[i]:= NSize;
  end;

  //set width of auto-sized columns
  for i:= 0 to Length(FColumnSizes)-1 do
  begin
    if FColumnSizes[i]=0 then
      FColumnWidths[i]:= Max(0, NTotalWidth-NFixedSize) div NAutoSized;
  end;
end;


procedure TATListbox.DoDefaultDrawItem(C: TCanvas; AIndex: integer; R: TRect);
var
  S, SItem: string;
  //NPos, not used
  NColOffset, NColWidth, NAllWidth, i: integer;
begin
  if AIndex=FItemIndex then
  begin
    C.Brush.Color:= ColorToRGB(FTheme^.ColorBgListboxSel);
    C.Font.Color:= ColorToRGB(FTheme^.ColorFontListboxSel);
  end
  else
  if FHotTrack and (AIndex=FHotTrackIndex) then
  begin
    C.Brush.Color:= ColorToRGB(FTheme^.ColorBgListboxHottrack);
    C.Font.Color:= ColorToRGB(FTheme^.ColorFontListbox);
  end
  else
  begin
    C.Brush.Color:= ColorToRGB(FTheme^.ColorBgListbox);
    C.Font.Color:= ColorToRGB(FTheme^.ColorFontListbox);
  end;
  C.FillRect(R);

  if (AIndex>=0) and (AIndex<FList.Count) then
    S:= FList[AIndex]
  else
    S:= '('+IntToStr(AIndex)+')';

  if Length(FColumnSizes)=0 then
  begin
    C.TextOut(
      R.Left+FIndentLeft,
      R.Top+FIndentTop,
      S);
  end
  else
  begin
    NAllWidth:= ClientWidth;
    NColOffset:= R.Left+FIndentLeft;
    C.Pen.Color:= Theme^.ColorSeparators;

    for i:= 0 to Length(FColumnSizes)-1 do
    begin
      NColWidth:= FColumnWidths[i];
      SItem:= SGetItem(S, FColumnSep);

      C.FillRect(
        Rect(NColOffset,
        R.Top,
        NAllWidth,
        R.Bottom)
        );
      C.TextOut(
        NColOffset+1,
        R.Top+FIndentTop,
        SItem
        );

      Inc(NColOffset, NColWidth);
      {$ifdef FPC}
      C.Line(NColOffset-1, R.Top, NColOffset-1, R.Bottom);
      {$else}
      C.MoveTo (NColOffset-1, R.Top);
      C.LineTo (NColOffset-1, R.Bottom);
      {$endif}
    end;
  end;
end;

procedure TATListbox.Paint;
var
  R: TRect;
begin

  inherited;
  UpdateScrollbar;
  UpdateItemHeight;
  UpdateColumnWidths;

  R:= ClientRect;
  if DoubleBuffered then
  begin
    FBitmap.Canvas.Font.Name:= FTheme^.FontName;
    FBitmap.Canvas.Font.Size:= FTheme^.DoScaleFont(FTheme^.FontSize);
    DoPaintTo(FBitmap.Canvas, R);
    Canvas.CopyRect(R, FBitmap.Canvas, R);
  end
  else
    DoPaintTo(Canvas, R);
end;

procedure TATListbox.Click;
var
  Pnt: TPoint;
begin
  if FCanGetFocus then
    {$ifdef FPC}
    LCLIntf.SetFocus(Handle);
    {$endif}

    {$ifndef FPC}
  if FCanGetFocus then
    SetFocus;
    {$endif}

  Pnt:= ScreenToClient(Mouse.CursorPos);
  ItemIndex:= GetItemIndexAt(Pnt);

  inherited; //OnClick must be after ItemIndex set
end;

function TATListbox.GetItemIndexAt(Pnt: TPoint): integer;
begin
  Result:= -1;
  if ItemCount=0 then exit;

  if (Pnt.X>=0) and (Pnt.X<ClientWidth) then
  begin
    Result:= Pnt.Y div FItemHeight + FItemTop;
    if Result>=ItemCount then
      Result:= -1;
  end;
end;

function TATListbox.ItemBottom: integer;
begin
  Result:= Min(ItemCount-1, FItemTop+GetVisibleItems-1);
end;

procedure TATListbox.ScrollbarChange(Sender: TObject);
begin
  ItemTop:= FScrollbar.Position;
end;

procedure TATListbox.SetCanBeFocused(AValue: boolean);
begin
  if FCanGetFocus=AValue then Exit;
  FCanGetFocus:= AValue;
  {$ifdef FPC}
  if AValue then
    ControlStyle:= ControlStyle-[csNoFocus]
  else
    ControlStyle:= ControlStyle+[csNoFocus];
  {$endif}
end;

procedure TATListbox.SetItemHeightPercents(AValue: integer);
begin
  if FItemHeightPercents=AValue then Exit;
  FItemHeightPercents:= AValue;
  FItemHeightIsFixed:= false;
end;

procedure TATListbox.SetVirtualItemCount(AValue: integer);
begin
  if FVirtualItemCount=AValue then Exit;
  if AValue<0 then Exit;
  FVirtualItemCount:= AValue;
  Scrolled;
  Invalidate;
end;

procedure TATListbox.SetItemIndex(AValue: integer);
begin
  if FItemIndex=AValue then Exit;
  if not IsIndexValid(AValue) then Exit;
  FItemIndex:= AValue;

  UpdateItemHeight; //needed, ItemHeight may be not calculated yet

  //scroll if needed
  if FItemIndex=0 then
    FItemTop:= 0
  else
  if FItemIndex<FItemTop then
    FItemTop:= FItemIndex
  else
  if FItemIndex>ItemBottom then
    FItemTop:= Max(0, FItemIndex-GetVisibleItems+1);

  ChangedSelection;
  Invalidate;
end;

procedure TATListbox.SetItemTop(AValue: integer);
begin
  if FItemTop=AValue then Exit;
  if not IsIndexValid(AValue) then Exit;
  FItemTop:= Max(0, AValue);
  Scrolled;
  Invalidate;
end;

procedure TATListbox.SetItemHeight(AValue: integer);
begin
  if AValue=FItemHeight then exit;
  FItemHeight:= AValue;
  FItemHeightIsFixed:= true;
end;

procedure TATListbox.SetThemedScrollbar(AValue: boolean);
begin
  if FThemedScrollbar=AValue then Exit;
  FThemedScrollbar:= AValue;

  FScrollbar.Visible:= AValue;
  Invalidate;
end;


constructor TATListbox.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle+[csOpaque] {$ifdef FPC}-[csTripleClicks]{$endif};
  DoubleBuffered:= IsDoubleBufferedNeeded;
  Width:= 180;
  Height:= 150;

  CanGetFocus:= false;
  FList:= TStringList.Create;
  FVirtualItemCount:= 0;
  FItemIndex:= 0;
  FItemHeightPercents:= 100;
  FItemHeight:= 17;
  FItemTop:= 0;
  FIndentLeft:= 4;
  FIndentTop:= 2;
  FOwnerDrawn:= false;
  FVirtualMode:= true;
  FHotTrack:= false;
  FColumnSep:= #9;
  SetLength(FColumnSizes, 0);
  SetLength(FColumnWidths, 0);

  FBitmap:= Graphics.TBitmap.Create;
  FBitmap.SetSize(1600, 1200);

  FTheme:= @ATFlatTheme;
  FThemedScrollbar:= true;

  FScrollbar:= TATScrollbar.Create(Self);
  FScrollbar.Parent:= Self;
  FScrollbar.Kind:= sbVertical;
  FScrollbar.Align:= alRight;

  FScrollbar.OnChange:= ScrollbarChange;

end;

destructor TATListbox.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FBitmap);
  inherited;
end;

{$ifdef FPC}
procedure TATListbox.UpdateFromScrollbarMsg(const Msg: TLMScroll);
var
  NMax: integer;
begin
  NMax:= Max(0, ItemCount-GetVisibleItems);

  case Msg.ScrollCode of
    SB_TOP:        FItemTop:= 0;
    SB_BOTTOM:     FItemTop:= Max(0, ItemCount-GetVisibleItems);

    SB_LINEUP:     FItemTop:= Max(0, FItemTop-1);
    SB_LINEDOWN:   FItemTop:= Min(NMax, FItemTop+1);

    SB_PAGEUP:     FItemTop:= Max(0, FItemTop-GetVisibleItems);
    SB_PAGEDOWN:   FItemTop:= Min(NMax, FItemTop+GetVisibleItems);

    SB_THUMBPOSITION,
    SB_THUMBTRACK: FItemTop:= Max(0, Msg.Pos);
  end;
end;
{$endif}

{$ifndef FPC}
procedure TATListbox.UpdateFromScrollbarMsg(const Msg: TWMVScroll);
var
  NMax: integer;
begin
  NMax:= Max(0, ItemCount-GetVisibleItems);

  case Msg.ScrollCode of
    SB_TOP:        FItemTop:= 0;
    SB_BOTTOM:     FItemTop:= Max(0, ItemCount-GetVisibleItems);

    SB_LINEUP:     FItemTop:= Max(0, FItemTop-1);
    SB_LINEDOWN:   FItemTop:= Min(NMax, FItemTop+1);

    SB_PAGEUP:     FItemTop:= Max(0, FItemTop-GetVisibleItems);
    SB_PAGEDOWN:   FItemTop:= Min(NMax, FItemTop+GetVisibleItems);

    SB_THUMBPOSITION,
    SB_THUMBTRACK: FItemTop:= Max(0, Msg.Pos);
  end;
end;

procedure TATListbox.CMMouseEnter(var msg: TMessage);
begin
  inherited;
  InvalidateNoSB;
end;

procedure TATListbox.CMMouseLeave(var msg: TMessage);
begin
  inherited;
  InvalidateNoSB;
end;

procedure TATListbox.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
  if Assigned(FScrollbar) then
    FScrollbar.Refresh;
end;
{$endif}

{$ifdef FPC}
procedure TATListbox.LMVScroll(var Msg: TLMVScroll);
begin
  UpdateFromScrollbarMsg(Msg);
  Invalidate;
end;
{$endif}

{$ifndef FPC}
procedure TATListbox.WMVScroll(var Msg: TWMVScroll);
begin
  UpdateFromScrollbarMsg(Msg);
  Invalidate;
end;

procedure TATListbox.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
 inherited;
 Message.Result:= Message.Result or DLGC_WANTARROWS;
end;

procedure TATListbox.WMKeyDown(var Message: TWMKeyDown);
var
  ShiftState: TShiftState;
begin

 { Check the ShiftState, like delphi does while processing WMKeyDown }
 ShiftState := KeyDataToShiftState(Message.KeyData);
 DoKeyDown(Message.CharCode,ShiftState);

 inherited;

end;

{$endif}

{$ifdef FPC}
function TATListbox.CanFocus: boolean;
begin
  Result:= FCanGetFocus;
end;
{$endif}

{$ifdef FPC}
function TATListbox.CanSetFocus: boolean;
begin
  Result:= FCanGetFocus;
end;
{$endif}

function TATListbox.ClientWidth: integer;
begin
  Result:= inherited ClientWidth;
  if ThemedScrollbar and FScrollbar.Visible then
    Dec(Result, FScrollbar.Width);
end;

procedure TATListbox.InvalidateNoSB;
var
  R: TRect;
begin
  if Assigned(FScrollbar) and FScrollbar.Visible then
  begin
    // https://github.com/Alexey-T/ATFlatControls/issues/32
    R:= Rect(0, 0, ClientWidth, ClientHeight);
    InvalidateRect(Handle, {$ifdef fpc}@{$endif}R, false);
  end
  else
    Invalidate;
end;


procedure TATListbox.Invalidate;
begin
  if Assigned(FScrollbar) then
    FScrollbar.Update;

  inherited Invalidate;
end;

function TATListbox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if not ThemedScrollbar then
  begin
    Result:= inherited;
    exit
  end;

  Result:= true;
  if WheelDelta>0 then
    ItemTop:= Max(0, ItemTop-Mouse.WheelScrollLines)
  else
    ItemTop:= Max(0, Min(ItemCount-VisibleItems, ItemTop+Mouse.WheelScrollLines));
end;

procedure TATListbox.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (key=vk_up) then
  begin
    ItemIndex:= ItemIndex-1;
    key:= 0;
    Exit
  end;
  if (key=vk_down) then
  begin
    ItemIndex:= ItemIndex+1;
    key:= 0;
    Exit
  end;

  if (key=vk_prior) then
  begin
    ItemIndex:= Max(0, ItemIndex-(VisibleItems-1));
    key:= 0;
    Exit
  end;
  if (key=vk_next) then
  begin
    ItemIndex:= Min(ItemCount-1, ItemIndex+(VisibleItems-1));
    key:= 0;
    Exit
  end;

  if (key=vk_home) then
  begin
    ItemIndex:= 0;
    key:= 0;
    Exit
  end;
  if (key=vk_end) then
  begin
    ItemIndex:= ItemCount-1;
    key:= 0;
    Exit
  end;

  if (key=vk_return) then
  begin
    DblClick;
    key:= 0;
    Exit
  end;
end;

procedure TATListbox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  DoKeyDown(Key,Shift);
end;

procedure TATListbox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewIndex: integer;
begin
  inherited;

  if FHotTrack then
  begin
    NewIndex:= GetItemIndexAt(Point(X, Y));
    if FHotTrackIndex<>NewIndex then
    begin
      FHotTrackIndex:= NewIndex;
      InvalidateNoSB;
    end;
  end
  else
    FHotTrackIndex:= -1;
end;

initialization

end.

