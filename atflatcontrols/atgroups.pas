{
ATGroups - several page-controls, each based on ATTabs
Copyright (c) Alexey Torgashin (UVViewSoft)
License: MPL 2.0 or LGPL
}

{$ifdef FPC}
  {$mode delphi}
{$else}
  {$define windows}
  {$define SP} //Allow using SpTBXLib
{$endif}

unit ATGroups;

interface

uses
  Classes, Forms, Types, Controls, Graphics,
  ExtCtrls, Menus,
  {$ifdef SP}
  SpTbxDkPanels, SpTbxItem,
  {$endif}
  ATTabs;

type
  TMySplitter = {$ifdef SP}TSpTbxSplitter{$else}TSplitter{$endif};
  TMyPopupMenu = {$ifdef SP} TSpTbxPopupMenu {$else} TPopupMenu {$endif};

type

  { TATPages }

  TATPages = class(TPanel)
  private
    FTabs: TATTabs;
    FEnabledEmpty: boolean;
    FOnTabFocus: TNotifyEvent;
    FOnTabClose: TATTabCloseEvent;
    FOnTabAdd: TNotifyEvent;
    FOnTabEmpty: TNotifyEvent;
    FOnTabOver: TATTabOverEvent;
    FOnTabMove: TATTabMoveEvent;
    FOnTabGetTick: TATTabGetTickEvent;
    procedure SetOnTabClose(AEvent: TATTabCloseEvent);
    procedure SetOnTabAdd(AEvent: TNotifyEvent);
    procedure TabClick(Sender: TObject);
    procedure TabDrawBefore(Sender: TObject;
      AType: TATTabElemType; ATabIndex: Integer;
      C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
    procedure TabEmpty(Sender: TObject);
    procedure TabOver(Sender: TObject; ATabIndex: Integer);
    procedure TabMove(Sender: TObject; NFrom, NTo: Integer);
    function TabGetTick(Sender: TObject; ATabObject: TObject): Int64;
  protected
    procedure Resize; override;
    procedure Click; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    function AddTab(AIndex: integer; AData: TATTabData; AndActivate: boolean=true): integer;
    function CloseTabsAll(AClosePinned: boolean): boolean;
    function CloseTabsOther(ATabIndex: Integer; AClosePinned, ADoRighter, ADoLefter: boolean): boolean;
    property Tabs: TATTabs read FTabs;
    property EnabledEmpty: boolean read FEnabledEmpty write FEnabledEmpty;
    property OnTabFocus: TNotifyEvent read FOnTabFocus write FOnTabFocus;
    property OnTabClose: TATTabCloseEvent read FOnTabClose write SetOnTabClose;
    property OnTabAdd: TNotifyEvent read FOnTabAdd write SetOnTabAdd;
    property OnTabEmpty: TNotifyEvent read FOnTabEmpty write FOnTabEmpty;
    property OnTabOver: TATTabOverEvent read FOnTabOver write FOnTabOver;
    property OnTabMove: TATTabMoveEvent read FOnTabMove write FOnTabMove;
    property OnTabGetTick: TATTabGetTickEvent read FOnTabGetTick write FOnTabGetTick;
  end;

type
  TATTabCloseId = (
    tabCloseCurrent,
    tabCloseOthersThisPage,
    tabCloseOthersAllPages,
    tabCloseLefterThisPage,
    tabCloseRighterThisPage,
    tabCloseAllThisPage,
    tabCloseAll
    );

type
  TATTabsStringOptionId = (
    tabOptionButtonLayout,
    tabOptionHintForX,
    tabOptionHintForPlus,
    tabOptionHintForArrowLeft,
    tabOptionHintForArrowRight,
    tabOptionHintForArrowMenu
    );

type
  TATTabsOptionId = (
    tabColorText,
    tabColorTextActive,
    tabColorTextModified,
    tabColorBg,
    tabColorBgActive,
    tabColorBgPassive,
    tabColorBgPassiveOver,
    tabColorBorderActive,
    tabColorBorderPassive,
    tabColorSeparator,
    tabColorCloseBg,
    tabColorCloseBgOver,
    tabColorCloseBorderOver,
    tabColorCloseX,
    tabColorCloseXOver,
    tabColorArrow,
    tabColorArrowOver,
    tabColorSpecMarks,
    tabColorActiveMark,
    tabOptionShowHint,
    tabOptionVarWidth,
    tabOptionMultiline,
    tabOptionScalePercents,
    tabOptionFontSize,
    tabOptionPosition,
    tabOptionShowFlat,
    tabOptionShowTabs,
    tabOptionShowXButtons,
    tabOptionShowXRounded,
    tabOptionShowPlus,
    tabOptionShowNums,
    tabOptionShowEntireColor,
    tabOptionMouseWheelMode,
    tabOptionDoubleClickClose,
    tabOptionMiddleClickClose,
    tabOptionDragDrop,
    tabOptionDragFromNotATTabs,
    tabOptionHeightInner,
    tabOptionWidthNormal,
    tabOptionWidthMin,
    tabOptionWidthMax,
    tabOptionSpacer,
    tabOptionSpacer2,
    tabOptionSpaceInitial,
    tabOptionSpaceBeforeText,
    tabOptionSpaceBetweenTabs,
    tabOptionSpaceSide,
    tabOptionFontScale,
    tabOptionColoredBandSize,
    tabOptionActiveMarkSize,
    tabOptionScrollMarkSizeX,
    tabOptionScrollMarkSizeY,
    tabOptionSpaceXRight,
    tabOptionSpaceXSize,
    tabOptionArrowSize,
    tabOptionButtonSize,
    tabOptionShowArrowsNear,
    tabOptionWhichActivateOnClose
    );

type
  TATGroupsMode = (
    gmUninited,
    gmOne,
    gm2v,
    gm2h,
    gm3v,
    gm3h,
    gm1plus2v,
    gm1plus2h,
    gm4v,
    gm4h,
    gm4grid,
    gm6v,
    gm6h,
    gm6grid
    );

const
  cGroupsCount: array[TATGroupsMode] of Integer = (
    1,
    1,
    2,
    2,
    3,
    3,
    3,
    3,
    4,
    4,
    4,
    6,
    6,
    6
    );

type
  TATGroupsNums = 0..5;

type
  TATGroupsPoints = array[TATGroupsNums] of TPoint;

type
  TATGroupsPopupEvent = procedure(Sender: TObject; APages: TATPages; ATabIndex: integer) of object;

type
  { TATGroups }

  TATGroups = class(TPanel)
  private
    FSplit1,
    FSplit2,
    FSplit3,
    FSplit4,
    FSplit5: TMySplitter;
    FPanel1,
    FPanel2: TPanel;
    FPos1,
    FPos2,
    FPos3,
    FPos4,
    FPos5: Double;
    FPrevWidth,
    FPrevHeight: Integer;
    FSplitPopup: TMyPopupMenu;
    FSplitW: integer;
    FMode: TATGroupsMode;
    FOnChangeMode: TNotifyEvent;
    FOnTabPopup: TATGroupsPopupEvent;
    FOnTabFocus: TNotifyEvent;
    FOnTabClose: TATTabCloseEvent;
    FOnTabAdd: TNotifyEvent;
    FOnTabOver: TATTabOverEvent;
    FOnTabMove: TATTabMoveEvent;
    FOnEmpty: TNotifyEvent;
    FOnTabGetTick: TATTabGetTickEvent;
    FPopupPages: TATPages;
    FPopupTabIndex: Integer;
    function GetImages: TImageList;
    procedure SetImages(AValue: TImageList);
    procedure SetSplitterMinSize(AValue: integer);
    procedure SetSplitterResizeStyle(AValue: TResizeStyle);
    procedure SetSplitterColor(AValue: TColor);
    procedure SplitterOnPaint(Sender: TObject);
    procedure TabFocus(Sender: TObject);
    procedure TabEmpty(Sender: TObject);
    procedure TabPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure TabClose(Sender: TObject; ATabIndex: Integer;
      var ACanClose, ACanContinue: boolean);
    procedure TabAdd(Sender: TObject);
    procedure TabOver(Sender: TObject; ATabIndex: Integer);
    procedure TabMove(Sender: TObject; NFrom, NTo: Integer);
    function TabGetTick(Sender: TObject; ATabObject: TObject): Int64;
    procedure SetMode(Value: TATGroupsMode);
    function GetMainPos: Integer;
    procedure SetMainPos(N: Integer);
    procedure Split1Moved(Sender: TObject);
    procedure Split2Moved(Sender: TObject);
    procedure Split3Moved(Sender: TObject);
    procedure Split4Moved(Sender: TObject);
    procedure Split5Moved(Sender: TObject);
    procedure SplitClick(Sender: TObject);
    procedure InitSplitterPopup;
    procedure MoveTabsOnModeChanging(Value: TATGroupsMode);
  protected
    procedure Resize; override;
  public
    Pages1,
    Pages2,
    Pages3,
    Pages4,
    Pages5,
    Pages6,
    PagesCurrent: TATPages;
    Pages: array[TATGroupsNums] of TATPages;
    IndexOfGroup: integer;
    //
    property Images: TImageList read GetImages write SetImages;
    property Panel1: TPanel read FPanel1;
    property Splitter1: TMySplitter read FSplit1;
    property Splitter2: TMySplitter read FSplit2;
    property Splitter3: TMySplitter read FSplit3;
    property Splitter4: TMySplitter read FSplit4;
    property Splitter5: TMySplitter read FSplit5;
    property SplitterResizeStyle: TResizeStyle write SetSplitterResizeStyle;
    property SplitterMinSize: integer write SetSplitterMinSize;
    property SplitterColor: TColor write SetSplitterColor;
    //
    constructor Create(AOwner: TComponent); override;
    procedure Invalidate; override;
    //
    function PagesVisibleCount: Integer;
    function PagesSetIndex(ANum: Integer): boolean;
    procedure PagesSetNext(ANext: boolean);
    function PagesNextIndex(AIndex: Integer; ANext: boolean; AEnableEmpty: boolean): Integer;
    function FindPages(APages: TATPages): Integer;
    procedure FindPositionOfControl(AObject: TObject; out APagesIndex, ATabIndex: Integer);
    procedure GetSizes(out APanelSize: TPoint; out APageSize: TATGroupsPoints);
    procedure SetSizes(const APanelSize: TPoint; const APageSize: TATGroupsPoints);
    //
    property PopupPages: TATPages read FPopupPages write FPopupPages;
    property PopupTabIndex: Integer read FPopupTabIndex write FPopupTabIndex;
    property SplitterPopupMenu: TMyPopupMenu read FSplitPopup;
    //
    property Mode: TATGroupsMode read FMode write SetMode;

    function GetTabTotalCount: Integer;
    function GetTabDataOfTotalIndex(N: Integer): TATTabData;
    function SetPagesAndTabIndex(APageIndex, ATabIndex: Integer): boolean;
    procedure SetTabOption(Id: TATTabsOptionId; N: Integer);
    procedure SetTabOptionString(Id: TATTabsStringOptionId; const AValue: string);
    procedure SetTabFont(AFont: TFont);
    function GetTabSingleRowHeight: integer;
    //
    function CloseTabs(Id: TATTabCloseId; AForPopupMenu, AClosePinned: boolean): boolean;
    //
    procedure MoveTab(AFromPages: TATPages; AFromIndex: Integer;
      AToPages: TATPages; AToIndex: Integer; AActivateTabAfter: boolean);
    procedure MoveTabsFromGroupToAnother(APagesFrom, APagesTo: TATPages);
    procedure MovePopupTabToNext(ANext: boolean);
    procedure MoveCurrentTabToNext(ANext: boolean);
    procedure MoveCurrentTabToOpposite;
    //
    property MainPos: Integer read GetMainPos write SetMainPos;
    procedure MainPosIncrease;
    procedure MainPosDecrease;
    procedure SaveSplitPos;
    procedure RestoreSplitPos;
    //
    property OnChangeMode: TNotifyEvent read FOnChangeMode write FOnChangeMode;
    property OnTabPopup: TATGroupsPopupEvent read FOnTabPopup write FOnTabPopup;
    property OnTabFocus: TNotifyEvent read FOnTabFocus write FOnTabFocus;
    property OnTabClose: TATTabCloseEvent read FOnTabClose write FOnTabClose;
    property OnTabAdd: TNotifyEvent read FOnTabAdd write FOnTabAdd;
    property OnTabOver: TATTabOverEvent read FOnTabOver write FOnTabOver;
    property OnTabMove: TATTabMoveEvent read FOnTabMove write FOnTabMove;
    property OnTabGetTick: TATTabGetTickEvent read FOnTabGetTick write FOnTabGetTick;
    property OnEmpty: TNotifyEvent read FOnEmpty write FOnEmpty;
  end;

function PtInControl(Control: TControl; const ScreenPnt: TPoint): boolean;

procedure DoControlLock(Ctl: TWinControl);
procedure DoControlUnlock(Ctl: TWinControl);

var
  MaxTabsHeightPercentsForMultiline: Integer = 70;

implementation

uses
  {$ifdef windows}
  Windows, Messages,
  {$endif}
  SysUtils, StrUtils,
  {$ifdef SP}
  SpTbxSkins,
  {$endif}
  {$ifdef FPC}
  LCLType,
  {$endif}
  Math, Dialogs;

const
  cAbsMin = 4;

procedure UpdW(C: TControl; Value: Integer); inline;
begin
  if C.Align<>alClient then
    if Value>cAbsMin then
      C.Width:= Value;
end;

procedure UpdH(C: TControl; Value: Integer); inline;
begin
  if C.Align<>alClient then
    if Value>cAbsMin then
      C.Height:= Value;
end;

function PtInControl(Control: TControl; const ScreenPnt: TPoint): boolean;
begin
  Result:= PtInRect(Control.ClientRect, Control.ScreenToClient(ScreenPnt));
end;

procedure DoControlLock(Ctl: TWinControl); inline;
begin
  ////If it's called, CudaText has bug: change group mode from 1 to 4Vert,
  ////and splitters show on wrong positions

  //Ctl.DisableAutoSizing;

  {$ifdef windows}
  Ctl.Perform(WM_SetRedraw, 0, 0);
  {$endif}
end;

procedure DoControlUnlock(Ctl: TWinControl); inline;
begin
  //Ctl.EnableAutoSizing;

  {$ifdef windows}
  Ctl.Perform(WM_SetRedraw, 1, 0);
  SetWindowPos(Ctl.Handle, 0, 0, 0, 0, 0,
    SWP_FRAMECHANGED or SWP_NOCOPYBITS or SWP_NOMOVE or SWP_NOZORDER or SWP_NOSIZE);
  {$endif}
end;


{ TATPages }

constructor TATPages.Create(AOwner: TComponent);
begin
  inherited;

  Caption:= '';
  BorderStyle:= bsNone;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  DragMode:= dmAutomatic;
  FEnabledEmpty:= true;

  Width:= 600;
  Height:= 600;

  FTabs:= TATTabs.Create(Self);
  FTabs.Parent:= Self;
  FTabs.Align:= alTop;
  FTabs.OnTabClick:= TabClick;
  FTabs.OnTabDrawBefore:= TabDrawBefore;
  FTabs.OnTabEmpty:= TabEmpty;
  FTabs.OnTabOver:= TabOver;
  FTabs.OnTabMove:= TabMove;
  FTabs.OnTabGetTick:= TabGetTick;

  //dont set FTabs.DragMode:=dmAutomatic, because then
  //DragNDrop is called (Lazarus) even with simple closing of tabs by X icon
  // http://synwrite.sourceforge.net/forums/viewtopic.php?f=20&t=2604

  FTabs.OptTabHeight:= 24;
  FTabs.OptSpacer:= 2;
  FTabs.OptSpacer2:= 4;
  FTabs.OptSpaceBetweenTabs:= 0;
  FTabs.OptSpaceXSize:= 14;
  FTabs.OptColoredBandSize:= 5;
  FTabs.OptTabWidthMinimal:= 40;
  FTabs.Height:= FTabs.OptTabHeight+FTabs.OptSpacer+1;

  //FTabs.OptShowModifiedText:= #$95;
  FTabs.OptMouseMiddleClickClose:= true;
  FTabs.OptMouseDoubleClickPlus:= true;

  FTabs.ColorBg:= clWindow;
  FTabs.ColorCloseX:= clDkGray;
end;

function TATPages.AddTab(AIndex: integer; AData: TATTabData;
  AndActivate: boolean): integer;
begin
  FTabs.AddTab(AIndex, AData);

  if AData.TabObject is TControl then
  begin
    TControl(AData.TabObject).Parent:= Self;
    TControl(AData.TabObject).Align:= alClient;
  end;

  if AIndex<0 then
    Result:= FTabs.TabCount-1
  else
    Result:= AIndex;

  if AndActivate then
    FTabs.TabIndex:= Result;
end;

function TATPages.CloseTabsAll(AClosePinned: boolean): boolean;
var
  Data: TATTabData;
  i: Integer;
begin
  Result:= false;
  Tabs.TabIndex:= 0; //activate 1st tab to remove TabIndex change on closing
  for i:= Tabs.TabCount-1 downto 0 do
  begin
    Data:= Tabs.GetTabData(i);
    if Assigned(Data) and Data.TabPinned and not AClosePinned then
      Continue;
    if not Tabs.DeleteTab(i, true, true) then
      Exit;
  end;
  Result:= true;
end;

function TATPages.CloseTabsOther(ATabIndex: Integer; AClosePinned, ADoRighter,
  ADoLefter: boolean): boolean;
var
  Data: TATTabData;
  i: Integer;
begin
  Result:= false;
  if ADoRighter then
    for i:= Tabs.TabCount-1 downto ATabIndex+1 do
    begin
      Data:= Tabs.GetTabData(i);
      if Assigned(Data) and Data.TabPinned and not AClosePinned then
        Continue;
      if not Tabs.DeleteTab(i, true, true) then
        Exit;
    end;
  if ADoLefter then
    for i:= ATabIndex-1 downto 0 do
    begin
      Data:= Tabs.GetTabData(i);
      if Assigned(Data) and Data.TabPinned and not AClosePinned then
        Continue;
      if not Tabs.DeleteTab(i, true, true) then
        Exit;
    end;
  Result:= true;
end;


procedure TATPages.TabClick(Sender: TObject);
var
  i: Integer;
  D: TATTabData;
  Ctl: TWinControl;
begin
  DoControlLock(Self);
  try
    for i:= 0 to FTabs.TabCount-1 do
    begin
      D:= FTabs.GetTabData(i);
      if D<>nil then
      begin
        Ctl:= D.TabObject as TWinControl;
        Ctl.Visible:= i=FTabs.TabIndex;
      end;
    end;
  finally
    DoControlUnlock(Self);
  end;

  D:= FTabs.GetTabData(FTabs.TabIndex);
  if D<>nil then
  begin
    Ctl:= D.TabObject as TWinControl;
    if Ctl.Showing then
      if Assigned(FOnTabFocus) then
        FOnTabFocus(FTabs);
  end;
end;

procedure TATPages.SetOnTabClose(AEvent: TATTabCloseEvent);
begin
  FOnTabClose:= AEvent;
  FTabs.OnTabClose:= AEvent;
end;

procedure TATPages.SetOnTabAdd(AEvent: TNotifyEvent);
begin
  FOnTabAdd:= AEvent;
  FTabs.OnTabPlusClick:= AEvent;
end;

procedure TATPages.TabEmpty(Sender: TObject);
begin
  if Assigned(FOnTabEmpty) then
    FOnTabEmpty(Sender);
end;

procedure TATPages.TabOver(Sender: TObject; ATabIndex: Integer);
begin
  if Assigned(FOnTabOver) then
    FOnTabOver(Sender, ATabIndex);
end;

procedure TATPages.TabMove(Sender: TObject; NFrom, NTo: Integer);
begin
  if Assigned(FOnTabMove) then
    FOnTabMove(Sender, NFrom, NTo);
end;

function TATPages.TabGetTick(Sender: TObject; ATabObject: TObject): Int64;
begin
  if Assigned(FOnTabGetTick) then
    Result:= FOnTabGetTick(Sender, ATabObject)
  else
    Result:= 0;
end;

procedure TATPages.Resize;
begin
  inherited;

  if Assigned(FTabs) then
    if (FTabs.OptPosition in [atpTop, atpBottom]) and FTabs.OptMultiline then
      FTabs.Constraints.MaxHeight:= Max(
        Height * MaxTabsHeightPercentsForMultiline div 100,
        FTabs.OptTabHeight
        );
end;

procedure TATPages.Click;
begin
  inherited;
  //click on empty area - add tab
  if FTabs.TabCount=0 then
    if Assigned(FOnTabAdd) then
      FOnTabAdd(FTabs);
end;

procedure TATPages.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  Accept:= Source is TATTabs;
end;

procedure TATPages.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Source is TATTabs then
    TATTabs(Source).DoTabDropToOtherControl(Self.Tabs, Types.Point(2, 2));
end;

{ TATGroups }

constructor TATGroups.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;

  Caption:= '';
  BorderStyle:= bsNone;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;

  Pages1:= TATPages.Create(Self);
  Pages2:= TATPages.Create(Self);
  Pages3:= TATPages.Create(Self);
  Pages4:= TATPages.Create(Self);
  Pages5:= TATPages.Create(Self);
  Pages6:= TATPages.Create(Self);

  Pages1.EnabledEmpty:= false;
  PagesCurrent:= Pages1;
  Pages[0]:= Pages1;
  Pages[1]:= Pages2;
  Pages[2]:= Pages3;
  Pages[3]:= Pages4;
  Pages[4]:= Pages5;
  Pages[5]:= Pages6;

  for i:= Low(Pages) to High(Pages) do
    with Pages[i] do
    begin
      Visible:= i=Low(Pages);
      Name:= 'aPages'+IntToStr(i); //debug
      Caption:= '';
      Tabs.Name:= 'aPagesTabs'+IntToStr(i); //debug
      //
      Parent:= Self;
      Align:= alLeft;
      //
      Tabs.OnContextPopup:= Self.TabPopup;
      OnTabEmpty:= Self.TabEmpty;
      OnTabFocus:= Self.TabFocus;
      OnTabClose:= Self.TabClose;
      OnTabAdd:= Self.TabAdd;
      OnTabOver:= Self.TabOver;
      OnTabMove:= Self.TabMove;
      OnTabGetTick:= Self.TabGetTick;
    end;

  FSplitW:= 5;

  FSplit1:= TMySplitter.Create(Self);
  FSplit1.Parent:= Self;
  FSplit1.OnMoved:= Split1Moved;

  FSplit2:= TMySplitter.Create(Self);
  FSplit2.Parent:= Self;
  FSplit2.OnMoved:= Split2Moved;

  FSplit3:= TMySplitter.Create(Self);
  FSplit3.Parent:= Self;
  FSplit3.OnMoved:= Split3Moved;

  FSplit4:= TMySplitter.Create(Self);
  FSplit4.Parent:= Self;
  FSplit4.OnMoved:= Split4Moved;

  FSplit5:= TMySplitter.Create(Self);
  FSplit5.Parent:= Self;
  FSplit5.OnMoved:= Split5Moved;

  SplitterResizeStyle:= rsPattern;
  SplitterMinSize:= 10;
  SplitterColor:= clMoneyGreen;

  FSplit1.Width:= FSplitW;
  FSplit2.Width:= FSplitW;
  FSplit3.Width:= FSplitW;
  FSplit4.Width:= FSplitW;
  FSplit5.Width:= FSplitW;

  {$ifdef fpc}
  FSplit1.AutoSnap:= false;
  FSplit2.AutoSnap:= false;
  FSplit3.AutoSnap:= false;
  FSplit4.AutoSnap:= false;
  FSplit5.AutoSnap:= false;
  {$endif}

  FPanel1:= TPanel.Create(Self);
  FPanel1.Parent:= Self;
  FPanel1.Align:= alTop;
  FPanel1.Caption:= '';
  FPanel1.BorderStyle:= bsNone;
  FPanel1.BevelInner:= bvNone;
  FPanel1.BevelOuter:= bvNone;
  FPanel1.Visible:= false;

  FPanel2:= TPanel.Create(Self);
  FPanel2.Parent:= Self;
  FPanel2.Align:= alClient;
  FPanel2.Caption:= '';
  FPanel2.BorderStyle:= bsNone;
  FPanel2.BevelInner:= bvNone;
  FPanel2.BevelOuter:= bvNone;
  FPanel2.Visible:= false;

  InitSplitterPopup;
  FPopupPages:= nil;
  FPopupTabIndex:= -1;
  FMode:= gmOne;
end;

procedure TATGroups.Invalidate;
var
  FPage: TATPages;
  i: integer;
begin
  inherited;

  for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
  begin
    FPage:= Pages[i];
    if Assigned(FPage) then
      FPage.Tabs.Invalidate;
  end;
end;

procedure TATGroups.InitSplitterPopup;
  //
  procedure Add(N: Integer);
  var
    MI: {$ifdef SP}TSpTbxItem{$else}TMenuItem{$endif};
  begin
    MI:= {$ifdef SP}TSpTbxItem{$else}TMenuItem{$endif}.Create(Self);
    MI.Caption:= Format('%d/%d', [N, 100-N]);
    MI.Tag:= N;
    MI.OnClick:= SplitClick;
    FSplitPopup.Items.Add(MI);
  end;
  //
begin
  FSplitPopup:= TMyPopupMenu.Create(Self);
  Add(20);
  Add(30);
  Add(40);
  Add(50);
  Add(60);
  Add(70);
  Add(80);
end;

procedure SetSplitterPopup(ASplitter: TMySplitter; APopup: TPopupMenu);
begin
  //some Delphi version dont publish TSplitter.PopupMenu
  ASplitter.PopupMenu:= APopup;
end;

procedure TATGroups.MoveTabsFromGroupToAnother(APagesFrom, APagesTo: TATPages);
var
  i: integer;
begin
  for i:= 0 to APagesFrom.Tabs.TabCount-1 do
    MoveTab(APagesFrom, 0{first tab}, APagesTo, -1, false);
end;

procedure TATGroups.MoveTabsOnModeChanging(Value: TATGroupsMode);
var
  NCountBefore, NCountAfter, i: Integer;
begin
  NCountBefore:= cGroupsCount[FMode];
  NCountAfter:= cGroupsCount[Value];

  //loop over group indexes, which will hide after mode changed
  for i:= NCountAfter to NCountBefore-1 do
    MoveTabsFromGroupToAnother(Pages[i], Pages[NCountAfter-1]);
end;

procedure TATGroups.SetMode(Value: TATGroupsMode);
var
  NSplit: Double;
  NPagesBefore, NPagesAfter: Integer;
  w, h, i: Integer;
begin
  if Value=gmUninited then
    Value:= gmOne;
  if (Value<>gmOne) and (Value=FMode) then Exit;

  w:= Width;
  h:= Height;
  if w<2 then exit;
  if h<2 then exit;

  try
    DoControlLock(Self);

    //actions before changing FMode
    NPagesBefore:= FindPages(PagesCurrent);
    MoveTabsOnModeChanging(Value);

    case FMode of
      gm2v:
        NSplit:= Pages1.Width / w;
      gm2h:
        NSplit:= Pages1.Height / h;
      else
        NSplit:= 0.5;
    end;

    //changing FMode and actions after changing
    FMode:= Value;

    SetSplitterPopup(FSplit1, nil);
    SetSplitterPopup(FSplit3, nil);
    case FMode of
      gm2v, gm2h:
        SetSplitterPopup(FSplit1, FSplitPopup);
      gm1plus2v, gm1plus2h:
        SetSplitterPopup(FSplit3, FSplitPopup);
    end;

    for i:= Low(Pages) to High(Pages) do
      Pages[i].Visible:= i<cGroupsCount[FMode];

    case FMode of
      gm1plus2v:
      begin
        FPanel1.Visible:= true;
        FPanel2.Visible:= true;
        Pages1.Parent:= FPanel1;
        Pages2.Parent:= FPanel2;
        Pages3.Parent:= FPanel2;
        Pages4.Parent:= FPanel2;
        Pages5.Parent:= FPanel2;
        Pages6.Parent:= FPanel2;
        FSplit1.Parent:= FPanel1;
        FSplit2.Parent:= FPanel2;
        FSplit3.Parent:= Self;
        FSplit4.Parent:= Self;
        FSplit5.Parent:= Self;
        //
        FPanel1.Align:= alLeft;
      end;
      gm1plus2h:
      begin
        FPanel1.Visible:= true;
        FPanel2.Visible:= true;
        Pages1.Parent:= FPanel1;
        Pages2.Parent:= FPanel2;
        Pages3.Parent:= FPanel2;
        Pages4.Parent:= FPanel2;
        Pages5.Parent:= FPanel2;
        Pages6.Parent:= FPanel2;
        FSplit1.Parent:= FPanel1;
        FSplit2.Parent:= FPanel2;
        FSplit3.Parent:= Self;
        FSplit4.Parent:= Self;
        FSplit5.Parent:= Self;
        //
        FPanel1.Align:= alTop;
      end;
      gm4grid:
      begin
        FPanel1.Visible:= true;
        FPanel2.Visible:= true;
        Pages1.Parent:= FPanel1;
        Pages2.Parent:= FPanel1;
        Pages3.Parent:= FPanel2;
        Pages4.Parent:= FPanel2;
        Pages5.Parent:= FPanel2;
        Pages6.Parent:= FPanel2;
        FSplit1.Parent:= FPanel1;
        FSplit2.Parent:= FPanel2;
        FSplit3.Parent:= Self;
        FSplit4.Parent:= Self;
        FSplit5.Parent:= Self;
        //
        FPanel1.Align:= alTop;
      end;
      gm6grid:
      begin
        FPanel1.Visible:= true;
        FPanel2.Visible:= true;
        Pages1.Parent:= FPanel1;
        Pages2.Parent:= FPanel1;
        Pages3.Parent:= FPanel1;
        Pages4.Parent:= FPanel2;
        Pages5.Parent:= FPanel2;
        Pages6.Parent:= FPanel2;
        FSplit1.Parent:= FPanel1;
        FSplit2.Parent:= FPanel1;
        FSplit3.Parent:= Self;
        FSplit4.Parent:= FPanel2;
        FSplit5.Parent:= FPanel2;
        //
        FPanel1.Align:= alTop;
      end
      else
      begin
        FPanel1.Visible:= false;
        FPanel2.Visible:= false;
        Pages1.Parent:= Self;
        Pages2.Parent:= Self;
        Pages3.Parent:= Self;
        Pages4.Parent:= Self;
        Pages5.Parent:= Self;
        Pages6.Parent:= Self;
        FSplit1.Parent:= Self;
        FSplit2.Parent:= Self;
        FSplit3.Parent:= Self;
        FSplit4.Parent:= Self;
        FSplit5.Parent:= Self;
      end;
    end;

    case FMode of
      gmOne:
        begin
          FSplit1.Visible:= false;
          FSplit2.Visible:= false;
          FSplit3.Visible:= false;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alClient;
        end;
      gm2v:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= false;
          FSplit3.Visible:= false;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alLeft;
          Pages2.Align:= alClient;
          FSplit1.Align:= alLeft;
          //size
          UpdW(Pages1, Trunc(w * NSplit));
          //pos
          Pages2.Left:= w;
          FSplit1.Left:= Pages2.Left;
        end;
      gm2h:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= false;
          FSplit3.Visible:= false;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alTop;
          Pages2.Align:= alClient;
          FSplit1.Align:= alTop;
          //size
          UpdH(Pages1, Trunc(h * NSplit));
          //pos
          Pages2.Top:= h;
          FSplit1.Top:= Pages2.Top;
        end;
      gm3v:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= false;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alLeft;
          Pages2.Align:= alLeft;
          Pages3.Align:= alClient;
          FSplit1.Align:= alLeft;
          FSplit2.Align:= alLeft;
          //size
          UpdW(Pages1, w div 3);
          UpdW(Pages2, w div 3);
          //pos
          Pages2.Left:= w;
          Pages3.Left:= w;
          FSplit1.Left:= Pages2.Left;
          FSplit2.Left:= Pages3.Left;
        end;
      gm3h:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= false;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alTop;
          Pages2.Align:= alTop;
          Pages3.Align:= alClient;
          FSplit1.Align:= alTop;
          FSplit2.Align:= alTop;
          //size
          UpdH(Pages1, h div 3);
          UpdH(Pages2, h div 3);
          //pos
          Pages2.Top:= h;
          Pages3.Top:= h;
          FSplit1.Top:= Pages2.Top;
          FSplit2.Top:= Pages3.Top;
        end;
      gm4v:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alLeft;
          Pages2.Align:= alLeft;
          Pages3.Align:= alLeft;
          Pages4.Align:= alClient;
          FSplit1.Align:= alLeft;
          FSplit2.Align:= alLeft;
          FSplit3.Align:= alLeft;
          //size
          UpdW(Pages1, w div 4);
          UpdW(Pages2, w div 4);
          UpdW(Pages3, w div 4);
          //pos
          Pages2.Left:= w;
          Pages3.Left:= w;
          Pages4.Left:= w;
          FSplit1.Left:= Pages2.Left;
          FSplit2.Left:= Pages3.Left;
          FSplit3.Left:= Pages4.Left;
        end;
      gm4h:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alTop;
          Pages2.Align:= alTop;
          Pages3.Align:= alTop;
          Pages4.Align:= alClient;
          FSplit1.Align:= alTop;
          FSplit2.Align:= alTop;
          FSplit3.Align:= alTop;
          //size
          UpdH(Pages1, h div 4);
          UpdH(Pages2, h div 4);
          UpdH(Pages3, h div 4);
          //pos
          Pages2.Top:= h;
          Pages3.Top:= h;
          Pages4.Top:= h;
          FSplit1.Top:= Pages2.Top;
          FSplit2.Top:= Pages3.Top;
          FSplit3.Top:= Pages4.Top;
        end;
      gm4grid:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alLeft;
          Pages2.Align:= alClient;
          Pages3.Align:= alLeft;
          Pages4.Align:= alClient;
          FSplit1.Align:= alLeft;
          FSplit2.Align:= alLeft;
          FSplit3.Align:= alTop;
          //size
          UpdW(Pages1, w div 2);
          UpdW(Pages3, w div 2);
          UpdH(FPanel1, h div 2);
          //pos-a
          FSplit1.Left:= w;
          Pages2.Left:= w;
          //pos-b
          FSplit2.Left:= w;
          Pages4.Left:= w;
          //pos-c
          FSplit3.Top:= h;
          FPanel2.Top:= h;
        end;
      gm1plus2v:
        begin
          FSplit1.Visible:= false;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alClient; //pages1 on panel1
          Pages2.Align:= alTop; //pages2 on panel2
          Pages3.Align:= alClient;
          Pages4.Align:= alBottom;
          FSplit1.Align:= alTop;
          FSplit2.Align:= alTop;
          FSplit3.Align:= alLeft;
          //size
          UpdH(Pages2, h div 2);
          UpdW(FPanel1, w div 2);
          //pos-b
          FSplit2.Top:= h;
          Pages4.Top:= h;
          //pos-c
          FSplit3.Left:= w;
          FPanel2.Left:= w;
        end;
      gm1plus2h:
        begin
          FSplit1.Visible:= false;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alClient; //pages1 on panel1
          Pages2.Align:= alLeft; //pages2 on panel2
          Pages3.Align:= alClient;
          Pages4.Align:= alRight;
          FSplit1.Align:= alLeft;
          FSplit2.Align:= alLeft;
          FSplit3.Align:= alTop;
          //size
          UpdW(Pages2, w div 2);
          UpdH(FPanel1, h div 2);
          //pos-b
          FSplit2.Left:= w;
          Pages4.Left:= w;
          //pos-c
          FSplit3.Top:= h;
          FPanel2.Top:= h;
        end;
      gm6grid:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= true;
          FSplit5.Visible:= true;
          Pages1.Align:= alLeft;
          Pages2.Align:= alLeft;
          Pages3.Align:= alClient;
          Pages4.Align:= alLeft;
          Pages5.Align:= alLeft;
          Pages6.Align:= alClient;
          FSplit1.Align:= alLeft;
          FSplit2.Align:= alLeft;
          FSplit3.Align:= alTop;
          FSplit4.Align:= alLeft;
          FSplit5.Align:= alLeft;
          //size
          UpdW(Pages1, w div 3);
          UpdW(Pages2, w div 3);
          UpdW(Pages4, w div 3);
          UpdW(Pages5, w div 3);
          UpdH(FPanel1, h div 2);
          //pos-a
          FSplit1.Left:= w;
          Pages2.Left:= w;
          FSplit2.Left:= w;
          Pages3.Left:= w;
          //pos-b
          FSplit4.Left:= w;
          Pages5.Left:= w;
          FSplit5.Left:= w;
          Pages6.Left:= w;
          //pos-c
          FSplit3.Top:= h;
          FPanel2.Top:= h;
        end;
      gm6v:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= true;
          FSplit5.Visible:= true;
          Pages1.Align:= alLeft;
          Pages2.Align:= alLeft;
          Pages3.Align:= alLeft;
          Pages4.Align:= alLeft;
          Pages5.Align:= alLeft;
          Pages6.Align:= alClient;
          FSplit1.Align:= alLeft;
          FSplit2.Align:= alLeft;
          FSplit3.Align:= alLeft;
          FSplit4.Align:= alLeft;
          FSplit5.Align:= alLeft;
          //size
          UpdW(Pages1, w div 6-3);
          UpdW(Pages2, w div 6-3);
          UpdW(Pages3, w div 6-3);
          UpdW(Pages4, w div 6-3);
          UpdW(Pages5, w div 6-3);
          //pos
          Pages2.Left:= w;
          Pages3.Left:= w;
          Pages4.Left:= w;
          Pages5.Left:= w;
          Pages6.Left:= w;
          FSplit1.Left:= Pages2.Left;
          FSplit2.Left:= Pages3.Left;
          FSplit3.Left:= Pages4.Left;
          FSplit4.Left:= Pages5.Left;
          FSplit5.Left:= Pages6.Left;
        end;
      gm6h:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= true;
          FSplit5.Visible:= true;
          Pages1.Align:= alTop;
          Pages2.Align:= alTop;
          Pages3.Align:= alTop;
          Pages4.Align:= alTop;
          Pages5.Align:= alTop;
          Pages6.Align:= alClient;
          FSplit1.Align:= alTop;
          FSplit2.Align:= alTop;
          FSplit3.Align:= alTop;
          FSplit4.Align:= alTop;
          FSplit5.Align:= alTop;
          //size
          UpdH(Pages1, h div 6-3);
          UpdH(Pages2, h div 6-3);
          UpdH(Pages3, h div 6-3);
          UpdH(Pages4, h div 6-3);
          UpdH(Pages5, h div 6-3);
          //pos
          Pages2.Top:= h;
          Pages3.Top:= h;
          Pages4.Top:= h;
          Pages5.Top:= h;
          Pages6.Top:= h;
          FSplit1.Top:= Pages2.Top;
          FSplit2.Top:= Pages3.Top;
          FSplit3.Top:= Pages4.Top;
          FSplit4.Top:= Pages5.Top;
          FSplit5.Top:= Pages6.Top;
        end;
    end;

    SaveSplitPos;

    //focus same group, if possible
    NPagesAfter:= Min(NPagesBefore, cGroupsCount[FMode]-1);
    if (NPagesAfter>=0) and (NPagesAfter<=High(TATGroupsNums)) then
    begin
      PagesCurrent:= Pages[NPagesAfter];
      if Assigned(FOnTabFocus) then
        FOnTabFocus(PagesCurrent.Tabs);
    end;
  finally
    DoControlUnlock(Self);
  end;

  if Assigned(FOnChangeMode) then
    FOnChangeMode(Self);
end;

procedure TATGroups.Split1Moved(Sender: TObject);
var
  R: double;
begin
  case FMode of
    gm3v:
      begin
        R:= FPos2/(1-FPos1);
        UpdW(Pages2, Trunc(R*(Width-Pages1.Width-2*FSplitW)));
      end;
    gm3h:
      begin
        R:= FPos2/(1-FPos1);
        UpdH(Pages2, Trunc(R*(Height-Pages1.Height-2*FSplitW)));
      end;
    gm4grid:
      begin
        UpdW(Pages3, Pages1.Width);
      end;
    gm6grid:
      begin
        R:= FPos2/(1-FPos1);
        UpdW(Pages2, Trunc(R*(Width-Pages1.Width-2*FSplitW)));
        UpdW(Pages4, Pages1.Width);
        UpdW(Pages5, Pages2.Width);
      end;
  end;

  SaveSplitPos;
end;

procedure TATGroups.Split2Moved(Sender: TObject);
{
var
  R: double;
  Size3: integer;
  }
begin
  case FMode of
    {
    //disabled due to https://github.com/Alexey-T/CudaText/issues/3199
    gm3v:
      begin
        Size3:= Pages3.Width;
        R:= FPos1/(FPos1+FPos2);
        UpdW(Pages1, Trunc(R*(Width-Size3)));
        UpdW(Pages2, Width-Pages1.Width-Size3-2*FSplitW);
      end;
    gm3h:
      begin
        Size3:= Pages3.Height;
        R:= FPos1/(FPos1+FPos2);
        UpdH(Pages1, Trunc(R*(Height-Size3)));
        UpdH(Pages2, Height-Pages1.Height-Size3-2*FSplitW);
      end;
      }
    gm6grid:
      begin
        {
        Size3:= Pages3.Width;
        R:= FPos1/(FPos1+FPos2);
        UpdW(Pages1, Trunc(R*(Width-Size3)));
        UpdW(Pages2, Width-Pages1.Width-Size3-2*FSplitW);
        }
        UpdW(Pages4, Pages1.Width);
        UpdW(Pages5, Pages2.Width);
      end;
    gm4grid:
      begin
        UpdW(Pages1, Pages3.Width);
      end;
  end;

  SaveSplitPos;
end;

procedure TATGroups.Split3Moved(Sender: TObject);
begin
  SaveSplitPos;
end;

procedure TATGroups.Split4Moved(Sender: TObject);
var
  R: double;
begin
  case FMode of
    gm6grid:
      begin
        R:= FPos2/(1-FPos1);
        UpdW(Pages5, Trunc(R*(Width-Pages4.Width-2*FSplitW)));
        UpdW(Pages1, Pages4.Width);
        UpdW(Pages2, Pages5.Width);
      end;
  end;

  SaveSplitPos;
end;

procedure TATGroups.Split5Moved(Sender: TObject);
{
var
  R: double;
  Size6: integer;
  }
begin
  case FMode of
    gm6grid:
      begin
        {
        //disabled due to https://github.com/Alexey-T/CudaText/issues/3199
        Size6:= Pages6.Width;
        R:= FPos1/(FPos1+FPos2);
        UpdW(Pages4, Trunc(R*(Width-Size6)));
        UpdW(Pages5, Width-Pages4.Width-Size6-2*FSplitW);
        }
        UpdW(Pages1, Pages4.Width);
        UpdW(Pages2, Pages5.Width);
      end;
  end;

  SaveSplitPos;
end;

procedure TATGroups.TabPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  Pnt, PntC: TPoint;
  IsX: boolean;
  i: Integer;
begin
  FPopupPages:= nil;
  FPopupTabIndex:= -1;

  Pnt:= (Sender as TControl).ClientToScreen(MousePos);
  for i:= Low(Pages) to High(Pages) do
    if PtInControl(Pages[i].Tabs, Pnt) then
    begin
      FPopupPages:= Pages[i];
      Break
    end;
  if FPopupPages=nil then Exit;

  PntC:= PopupPages.Tabs.ScreenToClient(Pnt);
  FPopupTabIndex:= FPopupPages.Tabs.GetTabAt(PntC.X, PntC.Y, IsX);

  if Assigned(FOnTabPopup) then
    FOnTabPopup(Self, FPopupPages, FPopupTabIndex);
  Handled:= true;
end;

procedure TATPages.TabDrawBefore(Sender: TObject;
  AType: TATTabElemType; ATabIndex: Integer;
  C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
begin
  {$ifndef SP}
  ACanDraw:= true;
  {$else}
  case AType of
    aeBackground:
    begin
      if SkinManager.CurrentSkinName='Default' then
      begin
        C.Brush.Color:= clBtnFace;
        C.FillRect(ARect);
      end
      else
        CurrentSkin.PaintBackground(C, ARect,
          skncDock, sknsNormal, true{BG}, false{Borders});
      ACanDraw:= false;
    end;

    aeXButton:
    begin
      //if ATabMouseOver then
      //  SpDrawXPToolbarButton(Control.Canvas, R, sknsHotTrack, cpNone);
      SpDrawGlyphPattern(C, ARect, 0{0 is X icon index},
        CurrentSkin.GetTextColor(skncToolbarItem, sknsNormal));
      ACanDraw:= false;
    end;

    aeXButtonOver:
    begin
      SpDrawXPToolbarButton(C,
        Rect(ARect.Left-1, ARect.Top-1, ARect.Right, ARect.Bottom),
        sknsHotTrack, cpNone);
      SpDrawGlyphPattern(C, ARect, 0{0 is X icon index},
        CurrentSkin.GetTextColor(skncToolbarItem, sknsNormal));
      ACanDraw:= false;
    end;
  end;
  {$endif}
end;

procedure TATGroups.SaveSplitPos;
var
  w, h: integer;
begin
  w:= Width;
  h:= Height;
  if w<2 then Exit;
  if h<2 then Exit;

  FPos1:= 0;
  FPos2:= 0;
  FPos3:= 0;
  FPos4:= 0;
  FPos5:= 0;

  case FMode of
    gm2v,
    gm3v,
    gm4v,
    gm6v:
      begin
        FPos1:= Pages1.Width / w;
        FPos2:= Pages2.Width / w;
        FPos3:= Pages3.Width / w;
        FPos4:= Pages4.Width / w;
        FPos5:= Pages5.Width / w;
      end;
    gm2h,
    gm3h,
    gm4h,
    gm6h:
      begin
        FPos1:= Pages1.Height / h;
        FPos2:= Pages2.Height / h;
        FPos3:= Pages3.Height / h;
        FPos4:= Pages4.Height / h;
        FPos5:= Pages5.Height / h;
      end;
    gm1plus2v:
      begin
        FPos1:= FPanel1.Width / w;
        FPos2:= Pages2.Height / h;
      end;
    gm1plus2h:
      begin
        FPos1:= FPanel1.Height / h;
        FPos2:= Pages2.Width / w;
      end;
    gm4grid:
      begin
        FPos1:= Pages1.Width / w;
        FPos2:= Pages3.Width / w;
        FPos3:= FPanel1.Height / h;
      end;
    gm6grid:
      begin
        FPos1:= Pages1.Width / w;
        FPos2:= Pages2.Width / w;
        FPos3:= FPanel1.Height / h;
        FPos4:= FPos1;
        FPos5:= FPos2;
      end;
  end;
end;

procedure TATGroups.RestoreSplitPos;
var
  w, h: integer;
begin
  w:= Width;
  h:= Height;
  if w<2 then Exit;
  if h<2 then Exit;

  case FMode of
    gm2v,
    gm3v,
    gm4v,
    gm6v:
      begin
        UpdW(Pages1, Trunc(FPos1 * w));
        UpdW(Pages2, Trunc(FPos2 * w));
        UpdW(Pages3, Trunc(FPos3 * w));
        UpdW(Pages4, Trunc(FPos4 * w));
        UpdW(Pages5, Trunc(FPos5 * w));
      end;
    gm2h,
    gm3h,
    gm4h,
    gm6h:
      begin
        UpdH(Pages1, Trunc(FPos1 * h));
        UpdH(Pages2, Trunc(FPos2 * h));
        UpdH(Pages3, Trunc(FPos3 * h));
        UpdH(Pages4, Trunc(FPos4 * h));
        UpdH(Pages5, Trunc(FPos5 * h));
      end;
    gm1plus2v:
      begin
        UpdW(FPanel1, Trunc(FPos1 * w));
        UpdH(Pages2, Trunc(FPos2 * h));
      end;
    gm1plus2h:
      begin
        UpdH(FPanel1, Trunc(FPos1 * h));
        UpdW(Pages2, Trunc(FPos2 * w));
      end;
    gm4grid:
      begin
        UpdW(Pages1, Trunc(FPos1 * w));
        UpdW(Pages3, Trunc(FPos2 * w));
        UpdH(FPanel1, Trunc(FPos3 * h));
      end;
    gm6grid:
      begin
        UpdW(Pages1, Trunc(FPos1 * w));
        UpdW(Pages2, Trunc(FPos2 * w));
        UpdH(FPanel1, Trunc(FPos3 * h));
        UpdW(Pages4, Pages1.Width);
        UpdW(Pages5, Pages2.Width);
      end;
  end;
end;

procedure TATGroups.Resize;
begin
  //Logic FPrev* needed for Lazarus!! laz calls onresize also for internal things like
  //splitter move and this causes bad things (resize group1 to width=0 in horz-view)
  if (FPrevWidth<>Width) or (FPrevHeight<>Height) then
  begin
    FPrevWidth:= Width;
    FPrevHeight:= Height;
    RestoreSplitPos;
  end;
end;


procedure TATGroups.TabEmpty(Sender: TObject);
var
  APages: TATPages;
begin
  APages:= (Sender as TATTabs).Parent as TATPages;

  //if last tab closed on Pages1, add new tab
  //if last tab closed on Pages2..Pages4, activate Pages1
  if not APages.EnabledEmpty then
  begin
    APages.OnTabAdd(Pages1.Tabs);
  end
  else
  begin
    if Pages1.Tabs.TabCount>0 then
      Pages1.Tabs.OnTabClick(nil);

    if Assigned(FOnEmpty) then
      FOnEmpty(APages);
  end;
end;

procedure TATGroups.SplitClick(Sender: TObject);
begin
  SetMainPos((Sender as TComponent).Tag);
end;

function TATGroups.GetMainPos: Integer;
//this is used when we have 2 cols or 2 rows, by context menu over single splitter
begin
  case FMode of
    gm2v:
      begin
        Result:= Pages1.Width * 100 div Width;
      end;
    gm2h:
      begin
        Result:= Pages1.Height * 100 div Height;
      end;
    gm1plus2v:
      begin
        Result:= FPanel1.Width * 100 div Width;
      end;
    gm1plus2h:
      begin
        Result:= FPanel1.Height * 100 div Height;
      end;
    else
      Result:= 50;
  end;
end;

procedure TATGroups.SetMainPos(N: Integer);
//this is used when we have 2 cols or 2 rows, by context menu over single splitter
begin
  case FMode of
    gm2v:
      begin
        UpdW(Pages1, Width * N div 100);
        SaveSplitPos;
      end;
    gm2h:
      begin
        UpdH(Pages1, Height * N div 100);
        SaveSplitPos;
      end;
    gm1plus2v:
      begin
        UpdW(FPanel1, Width * N div 100);
        SaveSplitPos;
      end;
    gm1plus2h:
      begin
        UpdH(FPanel1, Height * N div 100);
        SaveSplitPos;
      end;
  end;
end;

procedure TATGroups.MoveTab(AFromPages: TATPages; AFromIndex: Integer;
  AToPages: TATPages; AToIndex: Integer; AActivateTabAfter: boolean);
var
  D: TATTabData;
begin
  D:= AFromPages.Tabs.GetTabData(AFromIndex);
  if D=nil then Exit;

  AToPages.AddTab(AToIndex, D);
  AFromPages.Tabs.DeleteTab(AFromIndex, false, false);

  if AActivateTabAfter then
  begin
    with AToPages.Tabs do
      TabIndex:= IfThen(AToIndex>=0, AToIndex, TabCount-1);
    PagesCurrent:= AToPages;
  end;
end;


function TATGroups.PagesSetIndex(ANum: Integer): boolean;
var
  FPages: TATPages;
begin
  if (ANum>=Low(Pages)) and (ANum<=High(Pages)) then
    FPages:= Pages[ANum]
  else
    FPages:= nil;

  Result:= (FPages<>nil) and FPages.Visible and (FPages.Tabs.TabCount>0);
  if Result then
    FPages.Tabs.OnTabClick(nil);
end;

procedure TATGroups.PagesSetNext(ANext: boolean);
var
  Num0, Num1: Integer;
begin
  Num0:= FindPages(PagesCurrent);
  if Num0<0 then Exit;
  Num1:= PagesNextIndex(Num0, ANext, false);
  if Num1<0 then Exit;
  PagesSetIndex(Num1);
end;


function TATGroups.FindPages(APages: TATPages): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= Low(Pages) to High(Pages) do
    if Pages[i] = APages then
    begin
      Result:= i;
      Exit
    end;
end;

function TATGroups.PagesNextIndex(AIndex: Integer; ANext: boolean;
  AEnableEmpty: boolean): Integer;
var
  N: Integer;
begin
  Result:= -1;
  N:= AIndex;

  repeat
    if ANext then Inc(N) else Dec(N);
    if N>High(Pages) then N:= Low(Pages) else
      if N<Low(Pages) then N:= High(Pages);

    if N=AIndex then Exit; //don't return same index

    if Pages[N].Visible then
      if (Pages[N].Tabs.TabCount>0) or AEnableEmpty then
      begin
        Result:= N;
        Exit
      end;
  until false;
end;


procedure TATGroups.TabFocus(Sender: TObject);
begin
  if Assigned(FOnTabFocus) then
    FOnTabFocus(Sender);
end;

procedure TATGroups.MovePopupTabToNext(ANext: boolean);
var
  NFrom, NTo: Integer;
  PagesTo: TATPages;
begin
  NFrom:= FindPages(PopupPages);
  if NFrom<0 then Exit;
  NTo:= PagesNextIndex(NFrom, ANext, true);
  if NTo<0 then Exit;
  PagesTo:= Pages[NTo];
  MoveTab(PopupPages, PopupTabIndex, PagesTo, -1, false);
end;

procedure TATGroups.MoveCurrentTabToNext(ANext: boolean);
var
  PagesTo: TATPages;
  NFrom, NTo: Integer;
begin
  NFrom:= FindPages(PagesCurrent);
  if NFrom<0 then Exit;
  NTo:= PagesNextIndex(NFrom, ANext, true);
  if NTo<0 then Exit;
  PagesTo:= Pages[NTo];
  MoveTab(PagesCurrent, PagesCurrent.Tabs.TabIndex, PagesTo, -1, true);
end;

procedure TATGroups.TabClose(Sender: TObject; ATabIndex: Integer;
  var ACanClose, ACanContinue: boolean);
begin
  //not needed
  //DoControlLock(Self);
  //try

  if Assigned(FOnTabClose) then
    FOnTabClose(Sender, ATabIndex, ACanClose, ACanContinue);

  //finally
    //DoControlUnlock(Self);
  //end;
end;

procedure TATGroups.TabAdd(Sender: TObject);
begin
  if Assigned(FOnTabAdd) then
    FOnTabAdd(Sender);
end;

procedure TATGroups.SetTabFont(AFont: TFont);
var
  i: Integer;
begin
  for i:= Low(Pages) to High(Pages) do
    Pages[i].Tabs.Font.Assign(AFont);
end;

function TATGroups.GetTabSingleRowHeight: integer;
begin
  with Pages1.Tabs do
    Result:= DoScale(OptTabHeight+OptSpacer)+1;
end;

procedure TATGroups.SetTabOptionString(Id: TATTabsStringOptionId; const AValue: string);
var
  i: Integer;
begin
  for i:= Low(Pages) to High(Pages) do
    with Pages[i].Tabs do
      case Id of
        tabOptionButtonLayout:
          OptButtonLayout:= AValue;
        tabOptionHintForX:
          OptHintForX:= AValue;
        tabOptionHintForPlus:
          OptHintForPlus:= AValue;
        tabOptionHintForArrowLeft:
          OptHintForArrowLeft:= AValue;
        tabOptionHintForArrowRight:
          OptHintForArrowRight:= AValue;
        tabOptionHintForArrowMenu:
          OptHintForArrowMenu:= AValue;
      end;
end;


procedure TATGroups.SetTabOption(Id: TATTabsOptionId; N: Integer);
var
  i: Integer;
begin
  for i:= Low(Pages) to High(Pages) do
    with Pages[i].Tabs do
      case Id of
        //
        tabColorBg:
          begin
            ColorBg:= N;
            Pages[i].Color:= N;
            FSplit1.Color:= N;
            FSplit2.Color:= N;
            FSplit3.Color:= N;
            FSplit4.Color:= N;
            FSplit5.Color:= N;
          end;
        tabColorBgActive: ColorTabActive:= N;
        tabColorBgPassive: ColorTabPassive:= N;
        tabColorBgPassiveOver: ColorTabOver:= N;
        tabColorText: ColorFont:= N;
        tabColorTextActive: ColorFontActive:= N;
        tabColorTextModified: ColorFontModified:= N;
        tabColorBorderActive: ColorBorderActive:= N;
        tabColorBorderPassive: ColorBorderPassive:= N;
        tabColorSeparator: ColorSeparator:= N;
        tabColorCloseBg: ColorCloseBg:= N;
        tabColorCloseBgOver: ColorCloseBgOver:= N;
        tabColorCloseBorderOver: ColorCloseBorderOver:= N;
        tabColorCloseX: ColorCloseX:= N;
        tabColorCloseXOver: ColorCloseXOver:= N;
        tabColorArrow: ColorArrow:= N;
        tabColorArrowOver: ColorArrowOver:= N;
        tabColorSpecMarks: begin ColorDropMark:= N; ColorScrollMark:= N; end;
        tabColorActiveMark: ColorActiveMark:= N;
        //
        tabOptionFontSize:
          begin
            Font.Size:= N;
            OptTabHeight:= Trunc(N * 1.8) + 8; //tested for sizes 8..38
            Height:= GetTabSingleRowHeight;
          end;
        //
        tabOptionPosition:
          begin
            OptPosition:= TATTabPosition(N);
            case OptPosition of
              atpTop:
                begin
                  Align:= alTop;
                  Height:= GetTabSingleRowHeight;
                end;
              atpBottom:
                begin
                  Align:= alBottom;
                  Height:= GetTabSingleRowHeight;
                end;
              atpLeft:
                begin
                  Align:= alLeft;
                  Width:= OptTabWidthNormal;
                end;
              atpRight:
                begin
                  Align:= alRight;
                  Width:= OptTabWidthNormal;
                end;
            end;
          end;

        tabOptionShowHint:         ShowHint:= Boolean(N);
        tabOptionVarWidth:         OptVarWidth:= Boolean(N);
        tabOptionMultiline:        OptMultiline:= Boolean(N);
        tabOptionScalePercents:    OptScalePercents:= N;
        tabOptionShowFlat:         OptShowFlat:= Boolean(N);
        tabOptionShowTabs:         Visible:= Boolean(N);
        tabOptionShowXButtons:     OptShowXButtons:= TATTabShowClose(N);
        tabOptionShowXRounded:     OptShowXRounded:= Boolean(N);
        tabOptionShowPlus:         OptShowPlusTab:= Boolean(N);
        tabOptionShowNums:         OptShowNumberPrefix:= IfThen(Boolean(N), '%d. ', '');
        tabOptionShowEntireColor:  OptShowEntireColor:= Boolean(N);
        tabOptionShowArrowsNear:   OptShowArrowsNear:= Boolean(N);
        tabOptionMouseWheelMode:   OptMouseWheelMode:= TATTabMouseWheelMode(N);
        tabOptionDoubleClickClose: OptMouseDoubleClickClose:= Boolean(N);
        tabOptionMiddleClickClose: OptMouseMiddleClickClose:= Boolean(N);
        tabOptionDragDrop:         OptMouseDragEnabled:= Boolean(N);
        tabOptionDragFromNotATTabs:OptMouseDragFromNotATTabs:= Boolean(N);
        tabOptionHeightInner:      OptTabHeight:= N;
        tabOptionWidthMin:         OptTabWidthMinimal:= N;
        tabOptionWidthMax:         OptTabWidthMaximal:= N;
        tabOptionWidthNormal:
          begin
            OptTabWidthNormal:= N;
            if OptPosition in [atpLeft, atpRight] then
              Width:= OptTabWidthNormal;
          end;
        tabOptionSpacer:           OptSpacer:= N;
        tabOptionSpacer2:          OptSpacer2:= N;
        tabOptionSpaceInitial:     OptSpaceInitial:= N;
        tabOptionSpaceBeforeText:  OptSpaceBeforeText:= N;
        tabOptionSpaceBetweenTabs: OptSpaceBetweenTabs:= N;
        tabOptionSpaceSide:        OptSpaceSide:= N;
        tabOptionFontScale:        OptFontScale:= N;
        tabOptionColoredBandSize:  OptColoredBandSize:= N;
        tabOptionActiveMarkSize:   OptActiveMarkSize:= N;
        tabOptionScrollMarkSizeX:  OptScrollMarkSizeX:= N;
        tabOptionScrollMarkSizeY:  OptScrollMarkSizeY:= N;
        tabOptionSpaceXRight:      OptSpaceXRight:= N;
        tabOptionSpaceXSize:       OptSpaceXSize:= N;
        tabOptionArrowSize:        OptArrowSize:= N;
        tabOptionButtonSize:       OptButtonSize:= N;
        tabOptionWhichActivateOnClose: OptWhichActivateOnClose:= TATTabActionOnClose(N);
      end;
end;

procedure TATGroups.MoveCurrentTabToOpposite;
var
  NFrom, NTo, NTabIndex: Integer;
begin
  NFrom:= FindPages(PagesCurrent);
  if NFrom<0 then Exit;
  if NFrom=0 then NTo:= 1 else NTo:= 0;

  NTabIndex:= Pages[NFrom].Tabs.TabIndex;
  if NTabIndex<0 then Exit;

  if (NTo>0) and (FMode<=gmOne) then
    SetMode(gm2v);

  MoveTab(Pages[NFrom], NTabIndex, Pages[NTo], -1, true);
end;

function TATGroups.GetTabTotalCount: Integer;
var
  i: Integer;
begin
  Result:= 0;
  for i:= Low(Pages) to High(Pages) do
    Inc(Result, Pages[i].Tabs.TabCount);
end;


function TATGroups.GetTabDataOfTotalIndex(N: Integer): TATTabData;
var
  i, Count: Integer;
begin
  Result:= nil;
  Count:= N;
  for i:= Low(Pages) to High(Pages) do
  begin
    if (Count>=0) and (Count<Pages[i].Tabs.TabCount) then
    begin
      Result:= Pages[i].Tabs.GetTabData(Count);
      Exit
    end;
    Dec(Count, Pages[i].Tabs.TabCount);
  end;
end;

function TATGroups.CloseTabs(Id: TATTabCloseId; AForPopupMenu, AClosePinned: boolean): boolean;
var
  NPagesIndex, NTabIndex, i: Integer;
begin
  Result:= false;

  //we need NPagesIndex for Id<>tabCloseAll
  if Id<>tabCloseAll then
  begin
    if AForPopupMenu then
    begin
      if not Assigned(PopupPages) then exit;
      NPagesIndex:= FindPages(PopupPages);
      NTabIndex:= PopupTabIndex;
    end
    else
    begin
      if not Assigned(PagesCurrent) then exit;
      NPagesIndex:= FindPages(PagesCurrent);
      NTabIndex:= PagesCurrent.Tabs.TabIndex;
    end;

    if (NPagesIndex<0) or (NPagesIndex>High(TATGroupsNums)) then exit;
  end;

  case Id of
    tabCloseCurrent:
      begin
        with Pages[NPagesIndex].Tabs do
          if not DeleteTab(NTabIndex, true, true) then Exit;
      end;
    tabCloseOthersThisPage:
      begin
        if not Pages[NPagesIndex].CloseTabsOther(NTabIndex, AClosePinned, true, true) then Exit;
      end;
    tabCloseLefterThisPage:
      begin
        if not Pages[NPagesIndex].CloseTabsOther(NTabIndex, AClosePinned, false, true) then Exit;
      end;
    tabCloseRighterThisPage:
      begin
        if not Pages[NPagesIndex].CloseTabsOther(NTabIndex, AClosePinned, true, false) then Exit;
      end;
    tabCloseOthersAllPages:
      begin
        for i:= High(Pages) downto Low(Pages) do
          if i=NPagesIndex then
          begin
            if not Pages[i].CloseTabsOther(NTabIndex, AClosePinned, true, true) then Exit;
          end
          else
          begin
            if not Pages[i].CloseTabsAll(AClosePinned) then Exit;
          end;
      end;
    tabCloseAllThisPage:
      begin
        if not Pages[NPagesIndex].CloseTabsAll(AClosePinned) then Exit;
      end;
    tabCloseAll:
      begin
        for i:= High(Pages) downto Low(Pages) do
          if not Pages[i].CloseTabsAll(AClosePinned) then Exit;
      end;
  end;

  Result:= true;
end;


const
  cMinSplitter = 10;
  cDeltaSplitter = 5;

procedure TATGroups.MainPosIncrease;
begin
  MainPos:= Min(MainPos + cDeltaSplitter, 100-cMinSplitter);
end;

procedure TATGroups.MainPosDecrease;
begin
  MainPos:= Max(MainPos - cDeltaSplitter, cMinSplitter);
end;

function TATGroups.PagesVisibleCount: Integer;
begin
  Result:= cGroupsCount[FMode];
end;

function TATGroups.SetPagesAndTabIndex(APageIndex, ATabIndex: Integer): boolean;
var
  Page: TATPages;
begin
  Result:= false;
  if not ((APageIndex>=0) and (APageIndex<PagesVisibleCount)) then exit;
  Page:= Pages[APageIndex];
  if not ((ATabIndex>=0) and (ATabIndex<Page.Tabs.TabCount)) then exit;
  Result:= true;
  if Page.Tabs.TabIndex<>ATabIndex then
    Page.Tabs.TabIndex:= ATabIndex;
end;

procedure TATGroups.TabOver(Sender: TObject; ATabIndex: Integer);
begin
  if Assigned(FOnTabOver) then
    FOnTabOver(Sender, ATabIndex);
end;

procedure TATGroups.TabMove(Sender: TObject; NFrom, NTo: Integer);
begin
  if Assigned(FOnTabMove) then
    FOnTabMove(Sender, NFrom, NTo);
end;

function TATGroups.TabGetTick(Sender: TObject; ATabObject: TObject): Int64;
begin
  if Assigned(FOnTabGetTick) then
    Result:= FOnTabGetTick(Sender, ATabObject)
  else
    Result:= 0;
end;


procedure TATGroups.FindPositionOfControl(AObject: TObject;
  out APagesIndex, ATabIndex: Integer);
var
  TempPages: TATPages;
  P: TWinControl;
begin
  APagesIndex:= -1;
  ATabIndex:= -1;
  if AObject=nil then Exit;

  P:= TWinControl(AObject).Parent;
  if Assigned(P) then
  begin
    TempPages:= P as TATPages;
    APagesIndex:= FindPages(TempPages);
    ATabIndex:= TempPages.Tabs.FindTabByObject(AObject);
  end
  else
  begin
    APagesIndex:= 0;
    ATabIndex:= 0;
  end;
end;

function TATGroups.GetImages: TImageList;
begin
  Result:= Pages1.Tabs.Images;
end;

procedure TATGroups.SetImages(AValue: TImageList);
var
  i: integer;
begin
  for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
    Pages[i].Tabs.Images:= AValue;
end;

procedure TATGroups.SetSplitterMinSize(AValue: integer);
begin
  FSplit1.MinSize:= AValue;
  FSplit2.MinSize:= AValue;
  FSplit3.MinSize:= AValue;
  FSplit4.MinSize:= AValue;
  FSplit5.MinSize:= AValue;
end;

procedure TATGroups.SetSplitterResizeStyle(AValue: TResizeStyle);
begin
  FSplit1.ResizeStyle:= AValue;
  FSplit2.ResizeStyle:= AValue;
  FSplit3.ResizeStyle:= AValue;
  FSplit4.ResizeStyle:= AValue;
  FSplit5.ResizeStyle:= AValue;
end;

procedure TATGroups.SetSplitterColor(AValue: TColor);
var
  Event: TNotifyEvent;
begin
  if AValue=clNone then
    Event:= nil
  else
    Event:= SplitterOnPaint;

  FSplit1.OnPaint:= Event;
  FSplit2.OnPaint:= Event;
  FSplit3.OnPaint:= Event;
  FSplit4.OnPaint:= Event;
  FSplit5.OnPaint:= Event;

  FSplit1.Color:= AValue;
  FSplit2.Color:= AValue;
  FSplit3.Color:= AValue;
  FSplit4.Color:= AValue;
  FSplit5.Color:= AValue;

  FSplit1.Invalidate;
  FSplit2.Invalidate;
  FSplit3.Invalidate;
  FSplit4.Invalidate;
  FSplit5.Invalidate;
end;

procedure TATGroups.SplitterOnPaint(Sender: TObject);
begin
  //empty to disable themed paint
end;

function _FixOdd(N: integer): integer; inline;
//this is to fix shifting of splitter pos, if position is saved to config / restored later
begin
  if Odd(N) then
    Result:= N+1
  else
    Result:= N;
end;

procedure TATGroups.GetSizes(out APanelSize: TPoint; out APageSize: TATGroupsPoints);
var
  i: integer;
begin
  if (Width<2) or (Height<2) then
  begin
    APanelSize.x:= 100;
    APanelSize.y:= 100;
    for i in TATGroupsNums do
    begin
      APageSize[i].x:= 100;
      APageSize[i].y:= 100;
    end;
    exit
  end;

  APanelSize.x:= _FixOdd(Panel1.Width * 100 div Width);
  APanelSize.y:= _FixOdd(Panel1.Height * 100 div Height);
  for i in TATGroupsNums do
  begin
    APageSize[i].x:= _FixOdd(Pages[i].Width * 100 div Width);
    APageSize[i].y:= _FixOdd(Pages[i].Height * 100 div Height);
  end;
end;

procedure TATGroups.SetSizes(const APanelSize: TPoint; const APageSize: TATGroupsPoints);
const
  cMaxSize = 8000; //to avoid SigFPE on Linux sometimes, when setting Height~~100K
var
  i: integer;
begin
  Panel1.Width := Min(cMaxSize, APanelSize.x * Width div 100);
  Panel1.Height:= Min(cMaxSize, APanelSize.y * Height div 100);
  for i in TATGroupsNums do
  begin
    Pages[i].Width:= Min(cMaxSize, APageSize[i].x * Width div 100);
    Pages[i].Height:= Min(cMaxSize, APageSize[i].y * Height div 100);
  end;
end;

end.
