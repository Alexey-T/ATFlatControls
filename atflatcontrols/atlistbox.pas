{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATListbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  Forms,
  LMessages,
  ATScrollBar,
  ATFlatThemes;

type
  TATListboxDrawItemEvent = procedure(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect) of object;

type
  { TATListbox }

  TATListbox = class(TCustomControl)
  private
    FThemedScrollbar: boolean;
    FThemedColors: boolean;
    FScrollbar: TATScroll;
    FOwnerDrawn: boolean;
    FVirtualMode: boolean;
    FVirtualItemCount: integer;
    FItemIndex: integer;
    FItemHeight: integer;
    FItemTop: integer;
    FBitmap: TBitmap;
    FCanGetFocus: boolean;
    FList: TStringList;
    FColorSelFont: TColor;
    FColorSelBack: TColor;
    FColorHotTrackBack: TColor;
    FHotTrack: boolean;
    FHotTrackIndex: integer;
    FIndentLeft: integer;
    FIndentTop: integer;
    FOnDrawItem: TATListboxDrawItemEvent;
    FOnChangeSel: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    procedure DoDefaultDrawItem(C: TCanvas; AIndex: integer; R: TRect);
    procedure DoPaintTo(C: TCanvas; r: TRect);
    function ItemBottom: integer;
    procedure ScrollbarChange(Sender: TObject);
    procedure SetCanBeFocused(AValue: boolean);
    procedure SetVirtualItemCount(AValue: integer);
    procedure SetItemIndex(AValue: integer);
    procedure SetItemTop(AValue: integer);
    procedure SetThemedScrollbar(AValue: boolean);
    procedure UpdateFromScrollbarMsg(const Msg: TLMScroll);
    procedure UpdateScrollbar;
    function GetVisibleItems: integer;
    function IsIndexValid(N: integer): boolean;
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure LMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
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
    function ItemCount: integer;
    property HotTrackIndex: integer read FHotTrackIndex;
    property VirtualItemCount: integer read FVirtualItemCount write SetVirtualItemCount;
    property VisibleItems: integer read GetVisibleItems;
    function GetItemIndexAt(Pnt: TPoint): integer;
    property ThemedScrollbar: boolean read FThemedScrollbar write SetThemedScrollbar;
    property ThemedColors: boolean read FThemedColors write FThemedColors;
    property Scrollbar: TATScroll read FScrollbar;
    function CanFocus: boolean; override;
    function CanSetFocus: boolean; override;
    function ClientWidth: integer;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property BorderSpacing;
    property CanGetFocus: boolean read FCanGetFocus write SetCanBeFocused default false;
    property Color;
    property ColorSelFont: TColor read FColorSelFont write FColorSelFont default clWhite;
    property ColorSelBack: TColor read FColorSelBack write FColorSelBack default clMedGray;
    property ColorHotTrackBack: TColor read FColorHotTrackBack write FColorHotTrackBack default clMoneyGreen;
    property DoubleBuffered;
    property Enabled;
    property Font;
    property HotTrack: boolean read FHotTrack write FHotTrack default false;
    property IndentLeft: integer read FIndentLeft write FIndentLeft default 4;
    property IndentTop: integer read FIndentTop write FIndentTop default 2;
    property ItemHeight: integer read FItemHeight write FItemHeight default 21;
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
  Math, Types,
  InterfaceBase, 
  LCLType, LCLIntf;

function IsDoubleBufferedNeeded: boolean;
begin
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
end;

{ TATListbox }

function TATListbox.GetVisibleItems: integer;
begin
  Result:= ClientHeight div FItemHeight;
end;

function TATListbox.IsIndexValid(N: integer): boolean;
begin
  Result:= (N>=0) and (N<ItemCount);
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
  C.Brush.Color:= ColorToRGB(Color);
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

procedure TATListbox.DoDefaultDrawItem(C: TCanvas; AIndex: integer; R: TRect);
var
  S: string;
begin
  if AIndex=FItemIndex then
  begin
    C.Brush.Color:= ColorToRGB(FColorSelBack);
    C.Font.Color:= ColorToRGB(FColorSelFont);
  end
  else
  if FHotTrack and (AIndex=FHotTrackIndex) then
  begin
    C.Brush.Color:= ColorToRGB(ColorHotTrackBack);
    C.Font.Color:= ColorToRGB(Self.Font.Color);
  end
  else
  begin
    C.Brush.Color:= ColorToRGB(Color);
    C.Font.Color:= ColorToRGB(Self.Font.Color);
  end;
  C.FillRect(R);

  if (AIndex>=0) and (AIndex<FList.Count) then
    S:= FList[AIndex]
  else
    S:= '('+IntToStr(AIndex)+')';

  C.TextOut(
    R.Left+FIndentLeft,
    R.Top+FIndentTop,
    S);
end;

procedure TATListbox.Paint;
var
  R: TRect;
begin
  inherited;
  UpdateScrollbar;

  R:= ClientRect;
  if DoubleBuffered then
  begin
    FBitmap.Canvas.Font.Assign(Self.Font);
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
    LCLIntf.SetFocus(Handle);

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
  if AValue then
    ControlStyle:= ControlStyle-[csNoFocus]
  else
    ControlStyle:= ControlStyle+[csNoFocus];
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

  //scroll if needed
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

  ControlStyle:= ControlStyle+[csOpaque]-[csTripleClicks];
  DoubleBuffered:= IsDoubleBufferedNeeded;
  Width:= 180;
  Height:= 150;

  Color:= clWhite;
  ColorSelFont:= clWhite;
  ColorSelBack:= clMedGray;
  ColorHotTrackBack:= clMoneyGreen;
  CanGetFocus:= false;
  FList:= TStringList.Create;
  FVirtualItemCount:= 0;
  FItemIndex:= 0;
  FItemHeight:= 21;
  FItemTop:= 0;
  FIndentLeft:= 4;
  FIndentTop:= 2;
  FOwnerDrawn:= false;
  FVirtualMode:= true;
  FHotTrack:= false;

  FBitmap:= TBitmap.Create;
  FBitmap.SetSize(1600, 1200);

  FThemedScrollbar:= true;
  FThemedColors:= false;

  FScrollbar:= TATScroll.Create(Self);
  FScrollbar.Parent:= Self;
  FScrollbar.Kind:= sbVertical;
  FScrollbar.Align:= alRight;
  FScrollbar.Width:= ATFlatTheme.ScrollbarSize;
  FScrollbar.IndentBorder:= 0;
  FScrollbar.OnChange:= @ScrollbarChange;
end;

destructor TATListbox.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FBitmap);
  inherited;
end;

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

procedure TATListbox.LMVScroll(var Msg: TLMVScroll);
begin
  UpdateFromScrollbarMsg(Msg);
  Invalidate;
end;

function TATListbox.CanFocus: boolean;
begin
  Result:= FCanGetFocus;
end;

function TATListbox.CanSetFocus: boolean;
begin
  Result:= FCanGetFocus;
end;

function TATListbox.ClientWidth: integer;
begin
  Result:= inherited ClientWidth;
  if ThemedScrollbar then
    Dec(Result, FScrollbar.Width);
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

procedure TATListbox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

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

procedure TATListbox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FHotTrack then
  begin
    FHotTrackIndex:= GetItemIndexAt(Point(X, Y));
    Invalidate;
  end
  else
    FHotTrackIndex:= -1;
end;

initialization

end.

