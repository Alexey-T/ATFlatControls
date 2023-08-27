{$mode delphi}

unit demoform;

interface

uses
  LclType, //mb_okcancel
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus,
  ATGroups, ATTabs, 
  ComCtrls;

type
  { TfmTest }
  TfmTest = class(TForm)
    mnuMode6H: TMenuItem;
    mnuMode6V: TMenuItem;
    mnuMode1: TMenuItem;
    mnuMode12H: TMenuItem;
    mnuMode2H: TMenuItem;
    mnuMode2V: TMenuItem;
    mnuMode3H: TMenuItem;
    mnuMode3V: TMenuItem;
    mnuMode4G: TMenuItem;
    mnuMode4H: TMenuItem;
    mnuMode4V: TMenuItem;
    mnuMode6: TMenuItem;
    modeMode12V: TMenuItem;
    PopupMenu1: TPopupMenu;
    mnuClose: TMenuItem;
    MainMenu1: TMainMenu;
    Mode1: TMenuItem;
    N1: TMenuItem;
    m1: TMenuItem;
    m2: TMenuItem;
    m3: TMenuItem;
    m4: TMenuItem;
    Tree: TTreeView;
    Focus1: TMenuItem;
    Next1: TMenuItem;
    N12: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    N41: TMenuItem;
    N2: TMenuItem;
    Pr1: TMenuItem;
    mNext: TMenuItem;
    mPre: TMenuItem;
    N3: TMenuItem;
    Movetab1: TMenuItem;
    tonext1: TMenuItem;
    toprev1: TMenuItem;
    group51: TMenuItem;
    group61: TMenuItem;
    togroup51: TMenuItem;
    togroup61: TMenuItem;
    N4: TMenuItem;
    toothergroup1: TMenuItem;
    mnuCloseThis: TMenuItem;
    mnuCloseOthSame: TMenuItem;
    mnuCloseOthAll: TMenuItem;
    mnuCloseAll: TMenuItem;
    mnuCloseAllThis: TMenuItem;
    mTree: TMenuItem;
    Status: TStatusBar;
    mnuCloseRt: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    mnuCloseLt: TMenuItem;
    mnuTreeToggle: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure mnuMode12HClick(Sender: TObject);
    procedure mnuMode6HClick(Sender: TObject);
    procedure mnuMode6VClick(Sender: TObject);
    procedure mnuTreeToggleClick(Sender: TObject);
    procedure mnuMode1Click(Sender: TObject);
    procedure mnuMode2VClick(Sender: TObject);
    procedure mnuMode2HClick(Sender: TObject);
    procedure mnuMode3VClick(Sender: TObject);
    procedure mnuMode3HClick(Sender: TObject);
    procedure mnuMode4VClick(Sender: TObject);
    procedure mnuMode4HClick(Sender: TObject);
    procedure mnuMode4GClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure m1Click(Sender: TObject);
    procedure m2Click(Sender: TObject);
    procedure m3Click(Sender: TObject);
    procedure m4Click(Sender: TObject);
    procedure TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Next1Click(Sender: TObject);
    procedure N12Click(Sender: TObject);
    procedure N21Click(Sender: TObject);
    procedure N31Click(Sender: TObject);
    procedure N41Click(Sender: TObject);
    procedure Pr1Click(Sender: TObject);
    procedure mNextClick(Sender: TObject);
    procedure mPreClick(Sender: TObject);
    procedure tonext1Click(Sender: TObject);
    procedure toprev1Click(Sender: TObject);
    procedure TreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure mnuMode6Click(Sender: TObject);
    procedure group51Click(Sender: TObject);
    procedure group61Click(Sender: TObject);
    procedure togroup51Click(Sender: TObject);
    procedure togroup61Click(Sender: TObject);
    procedure toothergroup1Click(Sender: TObject);
    procedure modeMode12VClick(Sender: TObject);
    procedure mnuCloseThisClick(Sender: TObject);
    procedure mnuCloseAllClick(Sender: TObject);
    procedure mnuCloseOthSameClick(Sender: TObject);
    procedure mnuCloseOthAllClick(Sender: TObject);
    procedure mnuCloseAllThisClick(Sender: TObject);
    procedure mnuCloseRtClick(Sender: TObject);
    procedure mnuCloseLtClick(Sender: TObject);
  private
    { Private declarations }
    procedure TabClose(Sender: TObject; ATabIndex: Integer;
      var ACanClose, ACanContinue: boolean);
    procedure TabAdd(Sender: TObject);
    procedure AddTab(APages: TATPages);
    procedure TabPopup(Sender: TObject; APages: TATPages; AIndex: integer);
    procedure TabFocus(Sender: TObject);
    procedure TabOver(Sender: TObject; N: Integer);
    procedure MoveTabTo(AIndex: Integer);
    procedure MemoFocus(Sender: TObject);
  public
    { Public declarations }
    Groups: TATGroups;
  end;

var
  fmTest: TfmTest;

implementation

{$R *.lfm}

procedure TfmTest.AddTab(APages: TATPages);
var
  F: TMemo;
  i: Integer;
  ch: Char;
  Data: TATTabData;
begin
  F:= TMemo.Create(Self);
  F.Visible:= false;
  F.Parent:= Self;
  F.ScrollBars:= ssBoth;
  F.BorderStyle:= bsNone;
  F.OnEnter:= MemoFocus;

  ch:= Chr(Ord('A')+Random(26));
  for i:= 0 to 1+Random(4) do
    F.Lines.Add(StringOfChar(ch, 2+Random(50)));

  Data:= TATTabData.Create(nil);
  Data.TabObject:= F;
  Data.TabCaption:= 'tab'+ch;
  APages.AddTab(-1, Data, false);
end;


procedure TfmTest.FormCreate(Sender: TObject);
begin
  Tree.FullExpand;
  {$ifdef SP}
  SkinManager.SetSkin('Aluminum');
  {$endif}

  Groups:= TATGroups.Create(Self);
  Groups.Parent:= Self;
  Groups.Align:= alClient;
  Groups.OnTabPopup:= TabPopup;
  Groups.OnTabFocus:= TabFocus;
  Groups.OnTabClose:= TabClose;
  Groups.OnTabAdd:= TabAdd;
  Groups.OnTabOver:= TabOver;

  Groups.SetTabOption(tabColorText, clBlack);
  Groups.SetTabOption(tabColorBgActive, clBtnFace);
  Groups.SetTabOption(tabColorBgPassive, clLtGray);
  Groups.SetTabOption(tabColorBgPassiveOver, clMedGray);
  Groups.SetTabOption(tabColorBorderActive, clMedGray);
  Groups.SetTabOption(tabColorBorderPassive, clMedGray);
  Groups.SetTabOption(tabOptionDragFromNotATTabs, 1);

  AddTab(Groups.Pages1);
  AddTab(Groups.Pages1);
  AddTab(Groups.Pages1);
  AddTab(Groups.Pages2);
  AddTab(Groups.Pages2);

  Groups.Pages1.Tabs.TabIndex:= 0;
  Groups.Pages2.Tabs.TabIndex:= 0;
end;

procedure TfmTest.mnuMode12HClick(Sender: TObject);
begin
  Groups.Mode:= gm1plus2h;
end;

procedure TfmTest.mnuMode6HClick(Sender: TObject);
begin
  Groups.Mode:= gm6h;
end;

procedure TfmTest.mnuMode6VClick(Sender: TObject);
begin
  Groups.Mode:= gm6v;
end;

procedure TfmTest.mnuTreeToggleClick(Sender: TObject);
begin
  with Tree do Visible:= not Visible;
end;

procedure TfmTest.TabAdd(Sender: TObject);
begin
  AddTab((Sender as TATTabs).Parent as TATPages);
end;

procedure TfmTest.TabClose(Sender: TObject; ATabIndex: Integer;
  var ACanClose, ACanContinue: boolean);
var
  D: TATTabData;
  Res: TModalResult;
  Btns: TMsgDlgButtons;
begin
  D:= (Sender as TATTabs).GetTabData(ATabIndex);

  if ACanContinue then
    Btns:= mbYesNoCancel
  else
    Btns:= mbOKCancel;
  Res:= MessageDlg('Close tab', 'Close: '+D.TabCaption, mtConfirmation, Btns, 0);
  
  ACanClose:= Res in [mrOk, mrYes];
  ACanContinue:= Res<>mrCancel;

  if ACanClose then
    D.TabObject.Free;
end;

procedure TfmTest.TabPopup(Sender: TObject; APages: TATPages; AIndex: integer);
var
  D: TATTabData;
  P: TPoint;
begin
  D:= Groups.PopupPages.Tabs.GetTabData(Groups.PopupTabIndex);
  if D=nil then Exit;
  mnuCloseThis.Caption:= 'this tab: '+D.TabCaption;

  P:= Mouse.CursorPos;
  PopupMenu1.Popup(P.X, P.Y);
end;

procedure TfmTest.mnuMode1Click(Sender: TObject);
begin
  Groups.Mode:= gmOne;
end;

procedure TfmTest.mnuMode2VClick(Sender: TObject);
begin
  Groups.Mode:= gm2v;
end;

procedure TfmTest.mnuMode2HClick(Sender: TObject);
begin
  Groups.Mode:= gm2h;
end;

procedure TfmTest.mnuMode3VClick(Sender: TObject);
begin
  Groups.Mode:= gm3v;
end;

procedure TfmTest.mnuMode3HClick(Sender: TObject);
begin
  Groups.Mode:= gm3h;
end;

procedure TfmTest.modeMode12VClick(Sender: TObject);
begin
  Groups.Mode:= gm1plus2v;
end;

procedure TfmTest.mnuMode4VClick(Sender: TObject);
begin
  Groups.Mode:= gm4v;
end;

procedure TfmTest.mnuMode4HClick(Sender: TObject);
begin
  Groups.Mode:= gm4h;
end;

procedure TfmTest.mnuMode4GClick(Sender: TObject);
begin
  Groups.Mode:= gm4grid;
end;

procedure TfmTest.mnuMode6Click(Sender: TObject);
begin
  Groups.Mode:= gm6grid;
end;

procedure TfmTest.FormShow(Sender: TObject);
begin
  Groups.Mode:= gm2v;
end;

procedure TfmTest.MoveTabTo(AIndex: Integer);
begin
  Groups.MoveTab(Groups.PopupPages, Groups.PopupTabIndex, Groups.Pages[AIndex], -1, false);
end;

procedure TfmTest.m1Click(Sender: TObject);
begin
  MoveTabTo(0);
end;

procedure TfmTest.m2Click(Sender: TObject);
begin
  MoveTabTo(1);
end;

procedure TfmTest.m3Click(Sender: TObject);
begin
  MoveTabTo(2);
end;

procedure TfmTest.m4Click(Sender: TObject);
begin
  MoveTabTo(3);
end;

procedure TfmTest.togroup51Click(Sender: TObject);
begin
  MoveTabTo(4);
end;

procedure TfmTest.togroup61Click(Sender: TObject);
begin
  MoveTabTo(5);
end;

procedure TfmTest.TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Tabs: TATTabs;
  N: TTreeNode;
  S: string;
begin
  if Source is TMemo then
    S:= 'text: '+(Source as TMemo).Lines[0]
  else
  if Source is TATTabs then
  begin
    Tabs:= Source as TATTabs;
    S:= 'tab: '+Tabs.GetTabData(Tabs.TabIndex).TabCaption;
  end;

  N:= Tree.GetNodeAt(X, Y);
  if N=nil then begin Beep; Exit end;
  Tree.Items.AddChild(N, S);
  Tree.FullExpand;
end;

procedure TfmTest.Next1Click(Sender: TObject);
begin
  Groups.PagesSetNext(true);
end;

procedure TfmTest.Pr1Click(Sender: TObject);
begin
  Groups.PagesSetNext(false);
end;

procedure TfmTest.N12Click(Sender: TObject);
begin
  if not Groups.PagesSetIndex(0) then Beep;
end;

procedure TfmTest.N21Click(Sender: TObject);
begin
  if not Groups.PagesSetIndex(1) then Beep;
end;

procedure TfmTest.N31Click(Sender: TObject);
begin
  if not Groups.PagesSetIndex(2) then Beep;
end;

procedure TfmTest.N41Click(Sender: TObject);
begin
  if not Groups.PagesSetIndex(3) then Beep;
end;

procedure TfmTest.group51Click(Sender: TObject);
begin
  if not Groups.PagesSetIndex(4) then Beep;
end;

procedure TfmTest.group61Click(Sender: TObject);
begin
  if not Groups.PagesSetIndex(5) then Beep;
end;

procedure TfmTest.mNextClick(Sender: TObject);
begin
  Groups.MovePopupTabToNext(true);
end;

procedure TfmTest.mPreClick(Sender: TObject);
begin
  Groups.MovePopupTabToNext(false);
end;

procedure TfmTest.TabFocus(Sender: TObject);
var
  D: TATTabData;
  Ctl: TWinControl;
begin
  D:= (Sender as TATTabs).GetTabData((Sender as TATTabs).TabIndex);
  if D<>nil then
    if D.TabObject is TWinControl then
    begin
      Ctl:= D.TabObject as TWinControl;
      Ctl.Visible:= true;
      if Ctl.Enabled and Ctl.Visible and Ctl.CanFocus then
        Ctl.SetFocus;
    end;
end;

procedure TfmTest.MemoFocus(Sender: TObject);
begin
  Groups.PagesCurrent:= (Sender as TMemo).Parent as TATPages;
  Caption:= Format('Group: %d', [Groups.FindPages(Groups.PagesCurrent)]);
end;

procedure TfmTest.tonext1Click(Sender: TObject);
begin
  Groups.MoveCurrentTabToNext(true);
end;

procedure TfmTest.toprev1Click(Sender: TObject);
begin
  Groups.MoveCurrentTabToNext(false);
end;

procedure TfmTest.TreeDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept:= true;
end;

procedure TfmTest.toothergroup1Click(Sender: TObject);
begin
  Groups.MoveCurrentTabToOpposite;
end;

procedure TfmTest.mnuCloseThisClick(Sender: TObject);
begin
  Groups.PopupPages.Tabs.DeleteTab(Groups.PopupTabIndex, true, true);
end;

procedure TfmTest.mnuCloseOthSameClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseOthersThisPage, true, true);
end;

procedure TfmTest.mnuCloseOthAllClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseOthersAllPages, true, true);
end;

procedure TfmTest.mnuCloseAllClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseAll, true, false);
end;

procedure TfmTest.mnuCloseAllThisClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseAllThisPage, true, true);
end;

procedure TfmTest.TabOver(Sender: TObject; N: Integer);
begin
  if N>=0 then
    Status.SimpleText:= 'Mouse over tab '+IntToStr(N)
  else
    Status.SimpleText:= '';  
end;

procedure TfmTest.mnuCloseRtClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseRighterThisPage, true, true);
end;

procedure TfmTest.mnuCloseLtClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseLefterThisPage, true, true);
end;


end.
