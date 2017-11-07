unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ATListbox;

type
  { TfmMain }

  TfmMain = class(TForm)
    chkOwnerDrawn: TCheckBox;
    chkThemedScroll: TCheckBox;
    Panel1: TPanel;
    procedure chkOwnerDrawnChange(Sender: TObject);
    procedure chkThemedScrollChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
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

  list.OnDrawItem:= @ListDraw;
  list.OnClick:= @ListClick;
  list.OnDblClick:= @ListDblClick;
  list.OnChangedSel:= @ListChSel;

  list.OwnerDrawn:= true;
  list.Color:= $e0e0e0;
  list.ItemCount:= 21;
  list.ItemHeight:= 26;

  ActiveControl:= list;
end;

procedure TfmMain.chkThemedScrollChange(Sender: TObject);
begin
  list.ThemedScrollbar:= chkThemedScroll.checked;
end;

procedure TfmMain.chkOwnerDrawnChange(Sender: TObject);
begin
  list.OwnerDrawn:= chkOwnerDrawn.Checked;
  list.Invalidate;
end;

procedure TfmMain.ListDraw(Sender: TObject; C: TCanvas; AIndex: integer;
  const ARect: TRect);
var
  S: string;
begin
  C.Brush.Color:= IfThen(AIndex=list.ItemIndex, clMedGray, list.Color);
  C.FillRect(ARect);

  C.Pen.Color:= clMedGray;
  C.Line(ARect.Left+2, ARect.Bottom-1, ARect.Right-2, ARect.Bottom-1);

  S:= 'item '+IntToStr(AIndex);
  C.Font.Color:= $F0 shl AIndex; //weird color
  C.Font.Size:= AIndex+5; //weird font size
  C.TextOut(ARect.Left+6+AIndex*6, (ARect.Top+ARect.Bottom-C.TextHeight(S)) div 2, S);
end;

procedure TfmMain.ListClick(Sender: TObject);
begin
  Beep;
  Caption:= 'Clicked: '+IntToStr(list.ItemIndex);
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

