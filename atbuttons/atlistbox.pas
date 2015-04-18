{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0
}

unit ATListbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  LMessages;

type
  TATListboxDrawItemEvent = procedure(Sender: TObject; AIndex: integer; const ARect: TRect) of object;

type
  { TATListbox }

  TATListbox = class(TCustomControl)
  private
    FOnClick: TNotifyEvent;
    FOnDrawItem: TATListboxDrawItemEvent;
    FItemCount,
    FItemIndex,
    FItemHeight,
    FItemTop: integer;
    function ItemBottom: integer;
    procedure SetItemCount(AValue: integer);
    procedure SetItemIndex(AValue: integer);
    procedure SetItemTop(AValue: integer);
    procedure UpdateFromScrollbarMsg(const Msg: TLMScroll);
    procedure UpdateScrollbar;
    function GetVisibleItems: integer;
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure LMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ItemTop: integer read FItemTop write SetItemTop;
    property ItemCount: integer read FItemCount write SetItemCount;
  published
    property ItemHeight: integer read FItemHeight write FItemHeight;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDrawItem: TATListboxDrawItemEvent read FOnDrawItem write FOnDrawItem;
  end;

implementation

uses
  Math, Types, LCLType, LCLIntf;

{ TATListbox }

function TATListbox.GetVisibleItems: integer;
begin
  Result:= ClientHeight div FItemHeight;
end;

procedure TATListbox.UpdateScrollbar;
var
  si: TScrollInfo;
begin
  FillChar(si{%H-}, SizeOf(si), 0);
  si.cbSize:= SizeOf(si);
  si.fMask:= SIF_ALL;
  si.nMin:= 0;
  si.nMax:= FItemCount;
  si.nPage:= GetVisibleItems;
  si.nPos:= FItemTop;
  SetScrollInfo(Handle, SB_VERT, si, True);
end;

procedure TATListbox.Paint;
var
  r: TRect;
  index: integer;
begin
  inherited;

  UpdateScrollbar;

  r:= ClientRect;
  Canvas.Brush.Color:= Color;
  Canvas.FillRect(r);

  for index:= FItemTop to FItemCount-1 do
  begin
    r.Top:= (index-FItemTop)*FItemHeight;
    r.Bottom:= r.Top+FItemHeight;
    r.Left:= 0;
    r.Right:= ClientWidth;
    if r.Top>=ClientHeight then Break;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(Self, index, r);
  end;
end;

procedure TATListbox.Click;
var
  P: TPoint;
begin
  inherited;

  P:= ScreenToClient(Mouse.CursorPos);
  ItemIndex:= P.Y div FItemHeight + FItemTop;

  if Assigned(FOnClick) then
    FOnClick(Self);
end;

function TATListbox.ItemBottom: integer;
begin
  Result:= Min(ItemCount-1, FItemTop+GetVisibleItems-1);
end;

procedure TATListbox.SetItemCount(AValue: integer);
begin
  if FItemCount=AValue then Exit;
  if AValue<0 then Exit;
  FItemCount:= AValue;
  Invalidate;
end;

procedure TATListbox.SetItemIndex(AValue: integer);
begin
  if FItemIndex=AValue then Exit;
  if (AValue<0) or (AValue>=FItemCount) then Exit;
  FItemIndex:= AValue;

  if FItemIndex<FItemTop then
    FItemTop:= FItemIndex
  else
  if FItemIndex>ItemBottom then
    FItemTop:= Max(0, FItemIndex-GetVisibleItems+1);

  Invalidate;
end;

procedure TATListbox.SetItemTop(AValue: integer);
begin
  if FItemTop=AValue then Exit;
  FItemTop:= AValue;
  Invalidate;
end;


constructor TATListbox.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle
    +[csOpaque, csNoFocus]
    -[csDoubleClicks, csTripleClicks];

  Width:= 150;
  Height:= 120;

  FOnClick:= nil;
  FOnDrawItem:= nil;

  Color:= clLtGray;
  FItemCount:= 0;
  FItemIndex:= 0;
  FItemHeight:= 28;
  FItemTop:= 0;
end;

destructor TATListbox.Destroy;
begin
  inherited;
end;

procedure TATListbox.UpdateFromScrollbarMsg(const Msg: TLMScroll);
var
  NMax: integer;
begin
  NMax:= Max(0, FItemCount-GetVisibleItems);

  case Msg.ScrollCode of
    SB_TOP:        FItemTop:= 0;
    SB_BOTTOM:     FItemTop:= Max(0, FItemCount-GetVisibleItems);

    SB_LINEUP:     FItemTop:= Max(0, FItemTop-1);
    SB_LINEDOWN:   FItemTop:= Min(NMax, FItemTop+1);

    SB_PAGEUP:     FItemTop:= Max(0, FItemTop-GetVisibleItems);
    SB_PAGEDOWN:   FItemTop:= Min(NMax, FItemTop+GetVisibleItems);

    SB_THUMBPOSITION,
    SB_THUMBTRACK: FItemTop:= Msg.Pos;
  end;
end;

procedure TATListbox.LMVScroll(var Msg: TLMVScroll);
begin
  UpdateFromScrollbarMsg(Msg);
  Invalidate;
end;

initialization

end.

