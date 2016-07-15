{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATListbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  LMessages;

type
  TATListboxDrawItemEvent = procedure(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect) of object;

type
  { TATListbox }

  TATListbox = class(TCustomControl)
  private
    FOnDrawItem: TATListboxDrawItemEvent;
    FItemCount,
    FItemIndex,
    FItemHeight,
    FItemTop: integer;
    FBitmap: TBitmap;
    FCanGetFocus: boolean;
    FList: TStringList;
    FShowScrollbar: boolean;
    procedure DoPaintTo(C: TCanvas; r: TRect);
    function ItemBottom: integer;
    procedure SetCanBeFocused(AValue: boolean);
    procedure SetItemCount(AValue: integer);
    procedure SetItemIndex(AValue: integer);
    procedure SetItemTop(AValue: integer);
    procedure UpdateFromScrollbarMsg(const Msg: TLMScroll);
    procedure UpdateScrollbar;
    function GetVisibleItems: integer;
    function IsIndexValid(N: integer): boolean;
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure LMVScroll(var Msg: TLMVScroll); message LM_VSCROLL;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function CanFocus: boolean; override;
    function CanSetFocus: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items: TStringList read FList;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ItemTop: integer read FItemTop write SetItemTop;
    property ItemCount: integer read FItemCount write SetItemCount;
    property VisibleItems: integer read GetVisibleItems;
  published
    property Align;
    property BorderStyle;
    property BorderSpacing;
    property CanGetFocus: boolean read FCanGetFocus write SetCanBeFocused;
    property Color;
    property DoubleBuffered;
    property Font;
    property ItemHeight: integer read FItemHeight write FItemHeight;
    property ShowScrollbar: boolean read FShowScrollbar write FShowScrollbar;
    property OnClick;
    property OnDblClick;
    property OnDrawItem: TATListboxDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
  end;

implementation

uses
  Math, Types, LCLType, LCLIntf;

function IsDoubleBufferedNeeded: boolean;
begin
  Result:= false;

  {$ifdef windows}
  exit(true);
  {$endif}

  {$ifdef darwin}
  exit(false);
  {$endif}

  {$ifdef linux}
  exit(false);
  {$endif}
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

procedure TATListbox.UpdateScrollbar;
var
  si: TScrollInfo;
begin
  FillChar(si{%H-}, SizeOf(si), 0);
  si.cbSize:= SizeOf(si);
  si.fMask:= SIF_ALL;
  si.nMin:= 0;

  if not FShowScrollbar then
  begin
    si.nMax:= 1;
    si.nPage:= 2;
    SetScrollInfo(Handle, SB_VERT, si, True);
    exit
  end;

  si.nMax:= FItemCount;
  si.nPage:= GetVisibleItems;
  si.nPos:= FItemTop;
  SetScrollInfo(Handle, SB_VERT, si, True);
end;


procedure TATListbox.DoPaintTo(C: TCanvas; r: TRect);
var
  Index: integer;
begin
  C.Brush.Color:= Color;
  C.FillRect(r);

  for Index:= FItemTop to FItemCount-1 do
  begin
    r.Top:= (Index-FItemTop)*FItemHeight;
    r.Bottom:= r.Top+FItemHeight;
    r.Left:= 0;
    r.Right:= ClientWidth;
    if r.Top>=ClientHeight then Break;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(Self, C, Index, r)
    else
    begin
      //default paint useless
      C.Pen.Color:= clGray;
      C.Line(r.Left, r.Bottom, r.Right, r.Bottom);
      C.Brush.Color:= Color;
      if Index=FItemIndex then
      begin
        C.Brush.Color:= clMedGray;
        C.FillRect(r);
      end;
      C.TextOut(r.Left+6, r.Top+2, '('+IntToStr(Index)+')');
    end;
  end;
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
  ItemIndex:= Pnt.Y div FItemHeight + FItemTop;

  inherited; //OnClick must be after ItemIndex set
end;

function TATListbox.ItemBottom: integer;
begin
  Result:= Min(ItemCount-1, FItemTop+GetVisibleItems-1);
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

procedure TATListbox.SetItemCount(AValue: integer);
begin
  if FItemCount=AValue then Exit;
  if AValue<0 then Exit;
  FItemCount:= AValue;
  Changed;
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

  Changed;
  Invalidate;
end;

procedure TATListbox.SetItemTop(AValue: integer);
begin
  if FItemTop=AValue then Exit;
  if not IsIndexValid(AValue) then Exit;
  FItemTop:= Max(0, AValue);
  Changed;
  Invalidate;
end;


constructor TATListbox.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle+[csOpaque]-[csTripleClicks];
  DoubleBuffered:= IsDoubleBufferedNeeded;
  Width:= 180;
  Height:= 150;

  Color:= clLtGray;
  CanGetFocus:= false;
  FOnDrawItem:= nil;
  FList:= TStringList.Create;
  FItemCount:= 0;
  FItemIndex:= 0;
  FItemHeight:= 28;
  FItemTop:= 0;
  FShowScrollbar:= true;

  FBitmap:= TBitmap.Create;
  FBitmap.SetSize(1600, 1200);

  DoubleBuffered:= true; //user reported it helps on Win xp
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
  NMax:= Max(0, FItemCount-GetVisibleItems);

  case Msg.ScrollCode of
    SB_TOP:        FItemTop:= 0;
    SB_BOTTOM:     FItemTop:= Max(0, FItemCount-GetVisibleItems);

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


initialization

end.

