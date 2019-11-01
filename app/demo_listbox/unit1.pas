unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus,
  ATListbox,
  ATScrollbar,
  ATFlatThemes;

type
  { TfmMain }

  TfmMain = class(TForm)
    ButtonGoto: TButton;
    chkDoubleSize: TCheckBox;
    chkHotTrack: TCheckBox;
    chkVirtual: TCheckBox;
    chkOwnerDrawn: TCheckBox;
    chkThemedScroll: TCheckBox;
    comboScrolls: TComboBox;
    ComboShowX: TComboBox;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    TrackScale: TTrackBar;
    procedure ButtonGotoClick(Sender: TObject);
    procedure chkDoubleSizeChange(Sender: TObject);
    procedure chkHotTrackChange(Sender: TObject);
    procedure chkOwnerDrawnChange(Sender: TObject);
    procedure chkThemedScrollChange(Sender: TObject);
    procedure chkVirtualChange(Sender: TObject);
    procedure comboScrollsChange(Sender: TObject);
    procedure ComboShowXChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure TrackScaleChange(Sender: TObject);
  private
    function ListCalcScrollWidth(Sender: TObject; C: TCanvas): integer;
    procedure ListClickX(Sender: TObject);
    { private declarations }
    procedure ListDraw(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
    procedure ListClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListChSel(Sender: TObject);
  public
    { public declarations }
    list: TATListbox;
  end;

var
  fmMain: TfmMain;

implementation

uses Math, LCLProc, LCLType;

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
begin
  list:= TATListbox.Create(Self);
  list.Parent:= Self;
  list.Align:= alClient;
  list.CanGetFocus:= true;
  list.PopupMenu:= PopupMenu1;

  list.OnDrawItem:= @ListDraw;
  list.OnClick:= @ListClick;
  list.OnClickXMark:=@ListClickX;
  list.OnDblClick:= @ListDblClick;
  list.OnChangedSel:= @ListChSel;
  list.OnCalcScrollWidth:=@ListCalcScrollWidth;

  list.OwnerDrawn:= true;
  list.Color:= $e0e0e0;
  list.VirtualItemCount:= 21;

  list.Items.AddObject('real item first', TATListboxItemProp.Create(0, true, ''));
  list.Items.Add('real item 1');
  list.Items.Add('real item 2');
  list.Items.AddObject('real item last', TATListboxItemProp.Create(0, true, ''));

  ActiveControl:= list;
end;

procedure TfmMain.MenuItem1Click(Sender: TObject);
begin
  ShowMessage('Test...');
end;

procedure TfmMain.TrackScaleChange(Sender: TObject);
begin
  ATFlatTheme.ScalePercents:= TrackScale.Position;
  ATScrollbarTheme.ScalePercents:= TrackScale.Position;
  List.Invalidate;
end;

function TfmMain.ListCalcScrollWidth(Sender: TObject; C: TCanvas): integer;
begin
  Result:= 300;
end;

procedure TfmMain.chkThemedScrollChange(Sender: TObject);
begin
  list.ThemedScrollbar:= chkThemedScroll.checked;
end;

procedure TfmMain.chkVirtualChange(Sender: TObject);
begin
  list.VirtualMode:= chkVirtual.Checked;
  list.Invalidate;
end;

procedure TfmMain.comboScrollsChange(Sender: TObject);
begin
  List.Scrollbars:= TScrollStyle(comboScrolls.ItemIndex);
  List.Invalidate;
end;

procedure TfmMain.ComboShowXChange(Sender: TObject);
begin
  List.ShowXMark:= TATListboxShowX(ComboShowX.ItemIndex);
  List.Invalidate;
end;

procedure TfmMain.chkOwnerDrawnChange(Sender: TObject);
begin
  list.OwnerDrawn:= chkOwnerDrawn.Checked;
  list.Invalidate;
end;

procedure TfmMain.chkHotTrackChange(Sender: TObject);
begin
  list.HotTrack:= chkHotTrack.Checked;
  list.Invalidate;
end;

procedure TfmMain.chkDoubleSizeChange(Sender: TObject);
begin
  if chkDoubleSize.Checked then
    List.ItemHeightPercents:= 200
  else
    List.ItemHeightPercents:= 100;
  List.Update;
end;

procedure TfmMain.ButtonGotoClick(Sender: TObject);
var
  S: string;
  N: integer;
begin
  S:= IntToStr(List.ItemTop);
  S:= InputBox('Go to', 'Scroll to item index (0-based):', S);
  N:= StrToIntDef(S, -1);
  if List.IsIndexValid(N) then
    List.ItemIndex:= N;
end;

procedure TfmMain.ListDraw(Sender: TObject; C: TCanvas; AIndex: integer;
  const ARect: TRect);
var
  S: string;
begin
  if AIndex=list.ItemIndex then
    C.Brush.Color:= $B08080
  else
  if list.HotTrack and (AIndex=list.HotTrackIndex) then
    C.Brush.Color:= clMoneyGreen
  else
    C.Brush.Color:= list.Color;

  C.FillRect(ARect);

  C.Pen.Color:= clMedGray;
  C.Line(ARect.Left+2, ARect.Bottom-1, ARect.Right-2, ARect.Bottom-1);

  if List.VirtualMode then
    S:= 'virtual item '+IntToStr(AIndex)
  else
    S:= List.Items[AIndex];

  C.Font.Color:= $F0 shl AIndex; //weird color
  C.Font.Size:= AIndex+5; //weird font size
  C.TextOut(ARect.Left+6+AIndex*6-list.ScrollHorz, (ARect.Top+ARect.Bottom-C.TextHeight(S)) div 2, S);
end;

procedure TfmMain.ListClick(Sender: TObject);
begin
  Caption:= 'Clicked item: '+IntToStr(list.ItemIndex);
end;

procedure TfmMain.ListClickX(Sender: TObject);
begin
  Caption:= 'Clicked x mark: '+IntToStr(list.ItemIndex);
end;

procedure TfmMain.ListDblClick(Sender: TObject);
begin
  Caption:= 'Dbl-clicked: '+IntToStr(list.ItemIndex);
end;

procedure TfmMain.ListChSel(Sender: TObject);
begin
  Caption:= 'Change-sel: '+IntToStr(list.ItemIndex);
end;


end.

