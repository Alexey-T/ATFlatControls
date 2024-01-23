unit formmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, attabs, StdCtrls, math, XPMan, Menus;

type
  TForm1 = class(TForm)
    chkFlat: TCheckBox;
    chkShowPlus: TCheckBox;
    chkAngled: TCheckBox;
    chkGap: TCheckBox;
    chkVarWidth: TCheckBox;
    chkMultiline: TCheckBox;
    Label1: TLabel;
    chkPosTop: TRadioButton;
    chkPosBtm: TRadioButton;
    chkPosLeft: TRadioButton;
    chkPosRight: TRadioButton;
    chkCenterCaption: TCheckBox;
    XPManifest1: TXPManifest;
    Label2: TLabel;
    cbThemeList: TComboBox;
    PopupMenu1: TPopupMenu;
    item11: TMenuItem;
    item21: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure chkFlatClick(Sender: TObject);
    procedure chkShowPlusClick(Sender: TObject);
    procedure chkAngledClick(Sender: TObject);
    procedure chkGapClick(Sender: TObject);
    procedure chkVarWidthClick(Sender: TObject);
    procedure chkMultilineClick(Sender: TObject);
    procedure chkCenterCaptionClick(Sender: TObject);
    procedure chkPosTopClick(Sender: TObject);
    procedure chkPosBtmClick(Sender: TObject);
    procedure chkPosLeftClick(Sender: TObject);
    procedure chkPosRightClick(Sender: TObject);
    procedure cbThemeListClick(Sender: TObject);
  private
    { Private declarations }
    procedure SetTheme(const SName: string);
  public
    { Public declarations }
    t: TATTabs;
    procedure TabPlusClick(Sender: TObject);
    procedure TabContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  dir: string;
  sr: TSearchRec;
begin
  t:= TATTabs.Create(Self);
  t.Parent:= Self;
  t.Align:= alTop;
  t.OptSpaceSide:= 0;
  t.OptTabHeight:= 40;
  t.Height:= 50;

  t.AddTab(-1, 'tab first', nil, true, clGreen);
  t.AddTab(-1, WideString('юникод строка ')+WideChar($1020)+WideChar($2020));
  t.AddTab(-1, 'tab'#10'multiline'#10'caption', nil, false, clYellow);

  t.OnTabPlusClick:= TabPlusClick;
  t.OnContextPopup:= TabContextPopup;

  t.ColorBg:= clWhite;
  t.OptMouseDragEnabled:= true;
  t.Invalidate;

  dir:= ExtractFileDir(ExtractFileDir(Application.ExeName))+
    '\'+'img_themes\';

  cbThemeList.Items.Clear;
  cbThemeList.Items.Add('No Theme');
  if FindFirst(dir + '*',faDirectory,sr)=0 then
    repeat
      if ((sr.Attr and faDirectory) = faDirectory) and
        (sr.Name <> '.') and
        (sr.Name <> '..') then
          cbThemeList.Items.Add(sr.Name);
    until FindNext(sr)<>0;
  FindClose(sr);
  cbThemeList.ItemIndex := 0;

end;

procedure TForm1.SetTheme(const SName: string);
var
  Data: TATTabTheme;
begin

  if not DirectoryExists(SName) then
  begin
    ShowMessage('Theme folder not found:'#10+SName);
    exit;
  end;

  Data.FileName_Left:= SName+'\'+'l.png';
  Data.FileName_Right:= SName+'\'+'r.png';
  Data.FileName_Center:= SName+'\'+'c.png';
  Data.FileName_LeftActive:= SName+'\'+'l_a.png';
  Data.FileName_RightActive:= SName+'\'+'r_a.png';
  Data.FileName_CenterActive:= SName+'\'+'c_a.png';
  Data.FileName_X:= SName+'\'+'x.png';
  Data.FileName_XActive:= SName+'\'+'x_a.png';
  Data.FileName_Plus:= SName+'\'+'pl.png';
  Data.FileName_PlusActive:= SName+'\'+'pl_a.png';
  Data.FileName_ArrowLeft:= SName+'\'+'ar_l.png';
  Data.FileName_ArrowLeftActive:= SName+'\'+'ar_l_a.png';
  Data.FileName_ArrowRight:= SName+'\'+'ar_r.png';
  Data.FileName_ArrowRightActive:= SName+'\'+'ar_r_a.png';
  Data.FileName_ArrowDown:= SName+'\'+'ar_d.png';
  Data.FileName_ArrowDownActive:= SName+'\'+'ar_d_a.png';

  Data.SpaceBetweenInPercentsOfSide:= 150;
  Data.IndentOfX:= 2;
    
  t.SetTheme(Data);
  t.Invalidate;
end;

procedure TForm1.TabContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: boolean);
var
  P: TPoint;
begin
  P:= Mouse.CursorPos;
  PopupMenu1.Popup(P.X, P.Y);
end;

procedure TForm1.TabPlusClick(Sender: TObject);
begin
  t.AddTab(-1, 'tab'+IntToStr(t.TabCount));
  t.Invalidate;
end;

procedure TForm1.chkFlatClick(Sender: TObject);
begin
  t.OptShowFlat:= chkFlat.Checked;
  t.Invalidate;
end;

procedure TForm1.chkShowPlusClick(Sender: TObject);
begin
  t.OptShowPlusTab:= chkShowPlus.Checked;
  t.Invalidate;
end;

procedure TForm1.cbThemeListClick(Sender: TObject);
var
  dir, themefolder: string;
begin

  dir := ExtractFileDir(ExtractFileDir(Application.ExeName))+
    '\'+'img_themes\';
  themefolder := cbThemeList.Items[cbThemeList.ItemIndex];

  if DirectoryExists(dir + themefolder) then
    SetTheme(dir + themefolder)
  else
  begin
    t.IsThemed := false;
    t.Invalidate;
  end;

end;

procedure TForm1.chkAngledClick(Sender: TObject);
begin
  t.OptSpaceSide:= IfThen(chkAngled.Checked, 15, 0);
  t.OptSpaceInitial:= IfThen(chkAngled.Checked, 10, 4);
  t.Invalidate;
end;

procedure TForm1.chkGapClick(Sender: TObject);
begin
  t.OptSpaceBetweenTabs:= IfThen(chkGap.Checked, 6, 0);
  t.Invalidate;
end;

procedure TForm1.chkVarWidthClick(Sender: TObject);
begin
  t.OptVarWidth:= chkVarWidth.Checked;
  t.Invalidate;
end;

procedure TForm1.chkMultilineClick(Sender: TObject);
begin
  t.OptMultiline:= chkMultiline.Checked;
  t.Invalidate;
end;

procedure TForm1.chkCenterCaptionClick(Sender: TObject);
begin
  if chkCenterCaption.Checked then
    t.OptCaptionAlignment:= taCenter
  else
    t.OptCaptionAlignment:= taLeftJustify;
  t.Invalidate;
end;

procedure TForm1.chkPosTopClick(Sender: TObject);
begin
  t.Align:= alTop;
  t.OptPosition:= atpTop;
  t.Height:= 50;
  t.Invalidate;
end;

procedure TForm1.chkPosBtmClick(Sender: TObject);
begin
  t.Align:= alBottom;
  t.OptPosition:= atpBottom;
  t.Height:= 50;
  t.Invalidate;
end;

procedure TForm1.chkPosLeftClick(Sender: TObject);
begin
  t.Align:= alLeft;
  t.OptPosition:= atpLeft;
  t.Width:= 140;
  t.Invalidate;
end;

procedure TForm1.chkPosRightClick(Sender: TObject);
begin
  t.Align:= alRight;
  t.OptPosition:= atpRight;
  t.Width:= 140;
  t.Invalidate;
end;

end.
