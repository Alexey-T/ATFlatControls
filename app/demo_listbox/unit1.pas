unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ATListbox;

type
  { TfmMain }

  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure ListDraw(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
    procedure ListClick(Sender: TObject);
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
  //list.CanGetFocus:= true;

  list.OnDrawItem:= @ListDraw;
  list.OnClick:= @ListClick;

  list.Color:= $e0e0e0;
  list.ItemCount:= 21;
end;

procedure TfmMain.ListDraw(Sender: TObject; C: TCanvas; AIndex: integer;
  const ARect: TRect);
begin
  C.Brush.Color:= IfThen(AIndex=list.ItemIndex, clMedGray, list.Color);
  C.FillRect(ARect);

  C.Pen.Color:= clMedGray;
  C.Line(ARect.Left+2, ARect.Bottom-1, ARect.Right-2, ARect.Bottom-1);

  C.TextOut(ARect.Left+6, ARect.Top+2, 'item '+inttostr(AIndex));
end;

procedure TfmMain.ListClick(Sender: TObject);
begin
  Beep;
  Caption:= 'Clicked: '+IntToStr(list.ItemIndex);
end;


end.

