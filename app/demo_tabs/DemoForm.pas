unit DemoForm;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  StdCtrls, LCLProc, LCLType, Spin, ComCtrls, Menus, FileUtil,
  ATCanvasPrimitives,
  attabs;

type
  { TForm1 }
  TForm1 = class(TForm)
    BarScale: TTrackBar;
    btnAdd: TButton;
    btnDel: TButton;
    btnColor: TButton;
    btnLeft: TButton;
    btnRight: TButton;
    btnStress: TButton;
    btnToggleSpecial: TButton;
    chkRounded: TCheckBox;
    chkAngled: TCheckBox;
    chkCenterCaption: TCheckBox;
    chkFill: TCheckBox;
    chkMultiline: TCheckBox;
    chkMultiline_Bottom: TCheckBox;
    chkNewNearCurrent: TCheckBox;
    chkNums_Bottom: TCheckBox;
    chkShowFlat: TCheckBox;
    chkShowFullColor: TCheckBox;
    chkShowPlus: TCheckBox;
    chkVarSize: TCheckBox;
    chkVarSize_Bottom: TCheckBox;
    comboIconPos: TComboBox;
    comboLayout: TComboBox;
    comboShowX: TComboBox;
    comboThemes: TComboBox;
    comboTruncate: TComboBox;
    comboWheelMode: TComboBox;
    edBetweenTabs: TSpinEdit;
    edInitialSpace: TSpinEdit;
    edBeforeText: TSpinEdit;
    edAfterText: TSpinEdit;
    EditInfo: TEdit;
    GroupBoxBtm: TGroupBox;
    GroupBoxTop: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelThemes: TLabel;
    labStatus: TLabel;
    btnModify: TButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure BarScaleChange(Sender: TObject);
    procedure btnStressClick(Sender: TObject);
    procedure btnThemeBlack1Click(Sender: TObject);
    procedure btnThemeBlue1Click(Sender: TObject);
    procedure btnThemeChromeClick(Sender: TObject);
    procedure btnToggleSpecialClick(Sender: TObject);
    procedure chkAngledChange(Sender: TObject);
    procedure chkCenterCaptionChange(Sender: TObject);
    procedure chkFillChange(Sender: TObject);
    procedure chkMultiline_BottomChange(Sender: TObject);
    procedure chkMultilineChange(Sender: TObject);
    procedure chkNums_BottomChange(Sender: TObject);
    procedure chkRoundedChange(Sender: TObject);
    procedure chkShowFlatChange(Sender: TObject);
    procedure chkShowFullColorChange(Sender: TObject);
    procedure chkShowPlusChange(Sender: TObject);
    procedure chkVarSizeChange(Sender: TObject);
    procedure chkVarSize_BottomChange(Sender: TObject);
    procedure comboThemesChange(Sender: TObject);
    procedure comboIconPosChange(Sender: TObject);
    procedure comboLayoutChange(Sender: TObject);
    procedure comboShowXChange(Sender: TObject);
    procedure comboTruncateChange(Sender: TObject);
    procedure comboWheelModeChange(Sender: TObject);
    procedure edAfterTextChange(Sender: TObject);
    procedure edBeforeTextChange(Sender: TObject);
    procedure edBetweenTabsChange(Sender: TObject);
    procedure edInitialSpaceChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure btnColorClick(Sender: TObject);
    procedure btnLeftClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure EditInfoChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnModifyClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    { Private declarations }
    LockEdit: boolean;
    DirThemes: string;
    tabTopHeight: integer;

    procedure SetTheme(const SName: string);
    procedure TabChangeQuery(Sender: TObject; ANewTabIndex: integer;
      var ACanChange: boolean);
    procedure TabClickUserButton(Sender: TObject; AIndex: integer);
    procedure TabCloseEvent(Sender: TObject; ATabIndex: Integer; var ACanClose,
      ACanContinue: boolean);
    procedure TabContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure TabDragging(Sender: TObject; AIndexFrom, AIndexTo: integer;
      var ACanDrop: boolean);
    procedure TabMove(Sender: TObject; NFrom, NTo: Integer);
    procedure TabClick(Sender: TObject);
    procedure TabPlusClick(Sender: TObject);
    procedure Tab2PlusClick(Sender: TObject);
    procedure Tab3PlusClick(Sender: TObject);
    procedure TabClose(Sender: TObject; ATabIndex: Integer;
      var ACanClose, ACanContinie: boolean);
    procedure TabDrawAfter_Bottom(Sender: TObject;
      AType: TATTabElemType; ATabIndex: Integer;
      C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
    procedure TabDrawAfter_Top(Sender: TObject;
      AType: TATTabElemType; ATabIndex: Integer;
      C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
    procedure TabDrawBefore_Bottom(Sender: TObject;
      AType: TATTabElemType; ATabIndex: Integer;
      C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
  public
    { Public declarations }
    t_top, t_fox, t_cust: TATTabs;
  end;

var
  Form1: TForm1;

implementation

uses
  Math, StrUtils;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  List: TStringList;
  S: string;
begin
  //avoid drag on click arrows
  Mouse.DragImmediate:= false;
  Mouse.DragThreshold:= 20;

  t_top:= TATTabs.Create(Self);
  t_top.Constraints.MaxHeight:= 200;
  t_top.Parent:= Self;
  t_top.Align:= alTop;
  t_top.ColorBg:= $a0e0a0;
  t_top.ColorFont:= $E0E0E0;
  t_top.OnTabClick:= TabClick;
  t_top.OnTabPlusClick:= TabPlusClick;
  t_top.OnTabChangeQuery:=TabChangeQuery;
  t_top.OnTabClose:= TabClose;
  t_top.OnTabMove:= TabMove;
  t_top.OnTabClose:= TabCloseEvent;
  t_top.OnTabDrawAfter:= TabDrawAfter_Top;
  t_top.OnTabClickUserButton:=TabClickUserButton;
  t_top.OnTabDragging:=TabDragging;
  t_top.OnContextPopup:=TabContextPopup;
  t_top.OptMouseDoubleClickPlus:= true;
  t_top.OptShowXButtons:= atbxShowAll;
  t_top.OptSpaceBetweenTabs:= 10;
  t_top.Height:= 48;
  t_top.OptTabHeight:= 38;
  t_top.Images:= ImageList1;
  t_top.OptVarWidth:= true;
  t_top.ShowHint:= true;
  //
  //t_top.DragMode:= dmAutomatic; //drag-drop must work with automatic too
  t_top.DragMode:= dmManual;
  //
  tabTopHeight:= t_top.Height;

  t_top.AddTab(-1, 'Tab'#10'multiline');
  t_top.AddTab(-1, 'Tab middle len', nil, false, clGreen, 1);
  t_top.GetTabData(t_top.TabCount-1).TabHint:= 'tab some hint';
  t_top.AddTab(-1, 'Tab ____________', nil, false, clBlue, 2);
  t_top.GetTabData(t_top.TabCount-1).TabHint:= 'tab another hint';
  t_top.AddTab(-1, 'I'#10'mulline', nil, false, clNone, 0);
  t_top.AddTab(-1, 'I');
  t_top.AddTab(-1, 'I');
  t_top.AddTab(-1, 'I');
  t_top.AddTab(-1, 'I');

  t_cust:= TATTabs.Create(Self);
  t_cust.Parent:= Self;
  t_cust.Align:= alBottom;
  t_cust.ColorFont:= clNavy;
  t_cust.Font.Size:= 12;
  t_cust.Height:= 56;
  t_cust.OnTabPlusClick:= Tab3PlusClick;
  t_cust.OnTabDrawBefore:= TabDrawBefore_Bottom;
  t_cust.OnTabDrawAfter:= TabDrawAfter_Bottom;
  t_cust.ColorBg:= $F9EADB;
  t_cust.ColorBorderActive:= clLime;
  t_cust.ColorBorderPassive:= clFuchsia;

  t_cust.OptButtonLayout:= '<,>';
  t_cust.OptSpaceSide:= 10;
  t_cust.OptShowArrowsNear:= false;
  t_cust.OptTabHeight:= 30;
  t_cust.OptTabWidthNormal:= 170;
  t_cust.OptSpaceInitial:= 10;
  t_cust.OptSpaceBetweenTabs:= 4;
  t_cust.OptSpacer:= 20;
  t_cust.OptSpaceXSize:= 15;
  t_cust.OptSpaceXInner:= 3;
  t_cust.OptPosition:= atpBottom;

  t_cust.AddTab(-1, 'Owner-draw', nil, false, clNone);
  t_cust.AddTab(-1, 'Tab wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww', nil, false, clGreen);
  t_cust.AddTab(-1, 'Last');

  t_fox:= TATTabs.Create(Self);
  t_fox.Parent:= Self;
  t_fox.Align:= alBottom;
  t_fox.Font.Size:= 8;

  t_fox.Height:= 42;
  t_fox.OptButtonLayout:= '';
  t_fox.OptSpaceBetweenTabs:= 8;
  t_fox.OptSpaceInitial:= 20;
  t_fox.OptSpacer:= 4;
  t_fox.OptSpaceXSize:= 13;
  t_fox.OptMouseDragEnabled:= true;
  t_fox.OptSpaceSide:= 10;
  t_fox.OnTabPlusClick:= Tab2PlusClick;

  t_fox.ColorFont:= clBlack;
  t_fox.ColorBg:= $F9EADB;
  t_fox.ColorBorderActive:= $ACA196;
  t_fox.ColorBorderPassive:= $ACA196;
  t_fox.ColorTabActive:= $FCF5ED;
  t_fox.ColorTabPassive:= $E0D3C7;
  t_fox.ColorTabOver:= $F2E4D7;
  t_fox.ColorCloseBg:= clNone;
  t_fox.ColorCloseBgOver:= $D5C9BD;
  t_fox.ColorCloseBorderOver:= $B0B0B0;
  t_fox.ColorCloseX:= $7B6E60;
  t_fox.ColorArrow:= $5C5751;
  t_fox.ColorArrowOver:= t_fox.ColorArrow;

  t_fox.AddTab(-1, 'Firefox');
  t_fox.AddTab(-1, 'A tab _____________________________________________________', nil, false, clGreen);
  t_fox.AddTab(-1, 'Tab middle len', nil, false, clBlue);

  DirThemes:= ExtractFileDir(ExtractFileDir(ExtractFileDir(Application.ExeName)))+DirectorySeparator+'img_themes';
  comboThemes.Enabled:= DirectoryExists(DirThemes);
  if comboThemes.Enabled then
  begin
    List:= TStringList.Create;
    try
      FindAllDirectories(List, DirThemes);
      List.Sort;
      for S in List do
        comboThemes.Items.Add(ExtractFileName(S));
    finally
      FreeAndNil(List);
    end;
  end
  else
    LabelThemes.Caption:= 'no folder: '+DirThemes;

  edInitialSpace.Value:= t_top.OptSpaceInitial;
  edBetweenTabs.Value:= t_top.OptSpaceBetweenTabs;
  edBeforeText.Value:= t_top.OptSpaceBeforeText;
  edAfterText.Value:= t_top.OptSpaceAfterText;
end;

procedure TForm1.btnStressClick(Sender: TObject);
var
  i: integer;
begin
  for i:= t_top.TabCount-1 downto 1 do
    t_top.DeleteTab(i, false, false);
  for i:= 1 to 1000 do
  begin
    t_top.AddTab(-1, IntToStr(i)+'_'+StringOfChar('a', 3+Random(i mod 15)) );
    if i mod 100 = 0 then
      Application.ProcessMessages;
  end;
  t_top.TabIndex:= t_top.TabCount-1;
end;

procedure TForm1.BarScaleChange(Sender: TObject);
begin
  t_top.OptScalePercents:= BarScale.Position;
  t_top.Height:= t_top.DoScale(tabTopHeight);
end;

procedure TForm1.btnThemeBlack1Click(Sender: TObject);
begin
  SetTheme('black_wide');
end;

procedure TForm1.btnThemeBlue1Click(Sender: TObject);
begin
  SetTheme('blue_simple');
end;

procedure TForm1.btnThemeChromeClick(Sender: TObject);
begin
  SetTheme('chrome');
end;

procedure TForm1.SetTheme(const SName: string);
var
  dir: string;
  Data: TATTabTheme;
begin
  dir:= DirThemes+DirectorySeparator+SName;
  if not DirectoryExists(dir) then
  begin
    ShowMessage('Theme folder not found:'#10+dir);
    exit;
  end;

  Data.FileName_Left:= dir+DirectorySeparator+'l.png';
  Data.FileName_Right:= dir+DirectorySeparator+'r.png';
  Data.FileName_Center:= dir+DirectorySeparator+'c.png';
  Data.FileName_LeftActive:= dir+DirectorySeparator+'l_a.png';
  Data.FileName_RightActive:= dir+DirectorySeparator+'r_a.png';
  Data.FileName_CenterActive:= dir+DirectorySeparator+'c_a.png';
  Data.FileName_X:= dir+DirectorySeparator+'x.png';
  Data.FileName_XActive:= dir+DirectorySeparator+'x_a.png';
  Data.FileName_Plus:= dir+DirectorySeparator+'pl.png';
  Data.FileName_PlusActive:= dir+DirectorySeparator+'pl_a.png';
  Data.FileName_ArrowLeft:= dir+DirectorySeparator+'ar_l.png';
  Data.FileName_ArrowLeftActive:= dir+DirectorySeparator+'ar_l_a.png';
  Data.FileName_ArrowRight:= dir+DirectorySeparator+'ar_r.png';
  Data.FileName_ArrowRightActive:= dir+DirectorySeparator+'ar_r_a.png';
  Data.FileName_ArrowDown:= dir+DirectorySeparator+'ar_d.png';
  Data.FileName_ArrowDownActive:= dir+DirectorySeparator+'ar_d_a.png';

  Data.SpaceBetweenInPercentsOfSide:= 150;
  Data.IndentOfX:= 1;

  t_top.SetTheme(Data);
  t_top.Invalidate;
end;

procedure TForm1.TabChangeQuery(Sender: TObject; ANewTabIndex: integer;
  var ACanChange: boolean);
begin
  ACanChange:= true;
  exit;

  //this is not used now:
  ACanChange:= Application.MessageBox(
    PChar('Change tab index to '+IntToStr(ANewTabIndex)+'?'),
    'Tab change',
    MB_OKCANCEL or MB_ICONQUESTION)= ID_OK;
end;

procedure TForm1.btnToggleSpecialClick(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t_top.GetTabData(t_top.tabIndex);
  if d=nil then Exit;

  d.TabSpecial:= not d.TabSpecial;
  if d.TabSpecial then
    d.TabFontStyle:= [fsItalic, fsBold]
  else
    d.TabFontStyle:= [];

  t_top.Invalidate;
end;

procedure TForm1.chkAngledChange(Sender: TObject);
begin
  t_top.OptSpaceSide:= IfThen(chkAngled.Checked, 10, 0);
  t_top.OptSpaceBetweenTabs:= IfThen(chkAngled.Checked, 10, 0);
  t_top.Invalidate;
end;

procedure TForm1.chkCenterCaptionChange(Sender: TObject);
begin
  if chkCenterCaption.Checked then
    t_top.OptCaptionAlignment:= taCenter
  else
    t_top.OptCaptionAlignment:= taLeftJustify;
  t_top.Invalidate;
end;

procedure TForm1.chkFillChange(Sender: TObject);
begin
  t_top.OptFillWidth:= chkFill.Checked;
  t_top.Invalidate;
end;

procedure TForm1.chkMultiline_BottomChange(Sender: TObject);
begin
  t_cust.OptMultiline:= chkMultiline_Bottom.Checked;
  t_cust.Invalidate;
end;

procedure TForm1.chkMultilineChange(Sender: TObject);
begin
  t_top.OptMultiline:= chkMultiline.Checked;
  t_top.Invalidate;
end;

procedure TForm1.chkNums_BottomChange(Sender: TObject);
begin
  t_cust.OptShowNumberPrefix:= IfThen(chkNums_Bottom.Checked, '%d. ', '');
  t_cust.Invalidate;
end;

procedure TForm1.chkRoundedChange(Sender: TObject);
begin
  t_top.OptTabRounded:= chkRounded.Checked;
  t_top.Invalidate;
end;

procedure TForm1.chkShowFlatChange(Sender: TObject);
begin
  t_top.OptShowFlat:= chkShowFlat.Checked;
  t_fox.OptShowFlat:= chkShowFlat.Checked;
  t_cust.OptShowFlat:= chkShowFlat.Checked;
  t_top.Invalidate;
  t_fox.Invalidate;
  t_cust.Invalidate;
end;

procedure TForm1.chkShowFullColorChange(Sender: TObject);
begin
  t_top.OptShowEntireColor:= chkShowFullColor.Checked;
  t_top.Invalidate;
end;

procedure TForm1.chkShowPlusChange(Sender: TObject);
begin
  t_top.OptShowPlusTab:= chkShowPlus.Checked;
  t_top.Invalidate;
end;

procedure TForm1.chkVarSizeChange(Sender: TObject);
begin
  t_top.OptVarWidth:= chkVarSize.Checked;
  t_top.Invalidate;
end;

procedure TForm1.chkVarSize_BottomChange(Sender: TObject);
begin
  t_cust.OptVarWidth:= chkVarSize_Bottom.Checked;
  t_cust.Invalidate;
end;

procedure TForm1.comboThemesChange(Sender: TObject);
begin
  SetTheme(comboThemes.Text);
end;

procedure TForm1.comboIconPosChange(Sender: TObject);
begin
  t_top.OptIconPosition:= TATTabIconPosition(comboIconPos.ItemIndex);
  t_top.Invalidate;
end;

procedure TForm1.comboLayoutChange(Sender: TObject);
begin
  t_top.OptButtonLayout:= comboLayout.Text;
  t_top.OptShowArrowsNear:= Pos('<>', t_top.OptButtonLayout)>0;
  t_top.Invalidate;
end;

procedure TForm1.comboShowXChange(Sender: TObject);
begin
  t_top.OptShowXButtons:= TATTabShowClose(comboShowX.ItemIndex);
  t_top.Invalidate;
end;

procedure TForm1.comboTruncateChange(Sender: TObject);
begin
  t_top.OptTruncateCaption:= TATCollapseStringMode(comboTruncate.ItemIndex);
  t_top.Invalidate;
end;

procedure TForm1.comboWheelModeChange(Sender: TObject);
begin
  t_top.OptMouseWheelMode:= TATTabMouseWheelMode(comboWheelMode.ItemIndex);
end;

procedure TForm1.edAfterTextChange(Sender: TObject);
begin
  t_top.OptSpaceAfterText:= edAfterText.Value;
  t_top.Invalidate;
end;

procedure TForm1.edBeforeTextChange(Sender: TObject);
begin
  t_top.OptSpaceBeforeText:= edBeforeText.Value;
  t_top.Invalidate;
end;

procedure TForm1.edBetweenTabsChange(Sender: TObject);
begin
  t_top.OptSpaceBetweenTabs:= edBetweenTabs.Value;
  t_top.Invalidate;
end;

procedure TForm1.edInitialSpaceChange(Sender: TObject);
begin
  t_top.OptSpaceInitial:= edInitialSpace.Value;
  t_top.Invalidate;
end;

procedure TForm1.btnAddClick(Sender: TObject);
var
  NIndex: integer;
begin
  if chkNewNearCurrent.Checked then
    NIndex:= t_top.TabIndex+1
  else
    NIndex:= -1;

  t_top.AddTab(NIndex,
    'test '+StringOfChar('n', Random(20)),
    nil,
    false,
    Random(65000)
    );

  if NIndex>=0 then
    t_top.TabIndex:= NIndex
  else
    t_top.TabIndex:= t_top.TabCount-1;
end;

procedure TForm1.btnDelClick(Sender: TObject);
begin
  t_top.DeleteTab(t_top.TabIndex, true, false);
end;

procedure TForm1.btnColorClick(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t_top.GetTabData(t_top.tabindex);
  d.TabColor:= Random(60000);
  t_top.Invalidate;
end;

procedure TForm1.btnLeftClick(Sender: TObject);
begin
  t_top.tabIndex:= t_top.TabIndex-1;
end;

procedure TForm1.btnRightClick(Sender: TObject);
begin
  t_top.tabIndex:= t_top.TabIndex+1;
end;

procedure TForm1.TabClick(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t_top.GetTabData(t_top.TabIndex);
  LockEdit:= true;
  if Assigned(d) then
    EditInfo.Text:= d.TabCaption
  else
    EditInfo.Text:= '';
  LockEdit:= false;
end;

procedure TForm1.TabPlusClick(Sender: TObject);
begin
  btnAdd.Click;
end;

procedure TForm1.Tab2PlusClick(Sender: TObject);
begin
  t_fox.AddTab(t_fox.TabIndex+1, 'test');
end;

procedure TForm1.Tab3PlusClick(Sender: TObject);
begin
  t_cust.AddTab(t_cust.TabIndex+1, 'test');
end;


procedure TForm1.TabClose(Sender: TObject; ATabIndex: Integer;
  var ACanClose, ACanContinie: boolean);
{
var
  d: TATTabData;
  s: string;
  }
begin
  {
  d:= (Sender as TATTabs).GetTabData(ATabIndex);
  if d=nil then Exit;
  s:= d.TabCaption;
  ACanClose:= Pos('Tab', s)>0;
  }
  ACanClose:= true;
end;

procedure TForm1.EditInfoChange(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t_top.GetTabData(t_top.tabIndex);
  if d=nil then Exit;
  if LockEdit then Exit;

  d.TabCaption:= EditInfo.Text;
  t_top.Invalidate;
end;


procedure TForm1.FormShow(Sender: TObject);
begin
end;

procedure TForm1.TabDrawAfter_Bottom(Sender: TObject;
  AType: TATTabElemType; ATabIndex: Integer;
  C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
begin
  if ATabIndex<0 then Exit;
  //C.Font.Name:= 'Arial';
  //C.Font.Size:= 9;
  //C.Font.Color:= clBlue;
  //C.TextOut((ARect.Left+ARect.Right) div 2 - 8, ARect.Top+1, Inttostr(ATabIndex));
end;

procedure TForm1.TabDrawAfter_Top(Sender: TObject; AType: TATTabElemType;
  ATabIndex: Integer; C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
begin
  case AType of
    aeButtonUser,
    aeButtonUserOver:
      begin
        C.Font.Color:= clYellow;
        C.Brush.Color:= IfThen(AType=aeButtonUserOver, clRed, clBlue);
        C.TextOut(ARect.Left, ARect.Top+3, '_'+IntToStr(ATabIndex));
      end;
  end;
end;

procedure TForm1.TabDrawBefore_Bottom(Sender: TObject;
  AType: TATTabElemType; ATabIndex: Integer;
  C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
var
  D: TATTabData;
begin
  case AType of
    {
    aeBackground:
    begin
      NColor:= C.Brush.Color;
      //C.Brush.Style:= bsFDiagonal;
      C.Brush.Color:= clNavy;
      C.FillRect(ARect);
      C.Brush.Color:= NColor;
      C.Brush.Style:= bsSolid;
      ACanDraw:= false;
    end;
    }

    aeTabIconX,
    aeTabIconXOver:
      begin
        C.Font.Name:= 'Courier';
        C.Font.Size:= 12;
        C.Font.Color:= IfThen(AType=aeTabIconX, $303030, clRed);
        C.TextOut(ARect.Left-3, ARect.Top-3, 'x');
        ACanDraw:= false;
      end;

    aeTabPassive,
    aeTabPassiveOver,
    aeTabActive:
      begin
        D:= (Sender as TATTabs).GetTabData(ATabIndex);
        C.Pen.Color:= clYellow;
        case AType of
          aeTabPassive:
            C.Brush.Color:= clGray;
          aeTabPassiveOver:
            C.Brush.Color:= $40b040;
          aeTabActive:
            C.Brush.Color:= clGreen;
        end;
        C.RoundRect(ARect, 22, 22);
        C.Font.Name:= 'Arial';
        C.Font.Size:= 10;
        C.Font.Color:= clWhite;
        C.TextOut(
          ARect.Left+4,
          (ARect.Top+ARect.Bottom-C.TextHeight('W')) div 2,
          Copy(D.TabCaption, 1, 10)
          );
        ACanDraw:= false;
      end;
  end;
end;

procedure TForm1.TabMove(Sender: TObject; NFrom, NTo: Integer);
var s: string;
begin
  if NFrom=-1 then s:= 'add at index '+IntToStr(NTo) else
    if NTo=-1 then s:= 'delete at index '+IntToStr(NFrom) else
      s:= Format('move from %d to %d', [NFrom, NTo]);
  labStatus.Caption:= 'Status: '+s;
end;

procedure TForm1.btnModifyClick(Sender: TObject);
var
  d: TATTabData;
begin
  d:= t_top.GetTabData(t_top.tabIndex);
  if d=nil then Exit;

  d.TabModified:= not d.TabModified;
  t_top.Invalidate;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  ShowMessage('Test');
end;

procedure TForm1.TabCloseEvent(Sender: TObject; ATabIndex: Integer;
  var ACanClose, ACanContinue: boolean);
begin
  ACanClose:= Application.MessageBox('Close this tab?', 'Demo',
    MB_OKCANCEL+MB_ICONQUESTION) = ID_OK;
end;

procedure TForm1.TabContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  P: TPoint;
begin
  P:= Mouse.CursorPos;
  PopupMenu1.PopUp(P.X, P.Y);
end;

procedure TForm1.TabDragging(Sender: TObject; AIndexFrom, AIndexTo: integer;
  var ACanDrop: boolean);
begin
  labStatus.Caption:= Format('OnTabDragging: %d -> %d', [AIndexFrom, AIndexTo]);
end;

procedure TForm1.TabClickUserButton(Sender: TObject; AIndex: integer);
begin
  ShowMessage('User button '+IntToStr(AIndex));
end;

end.
