unit DemoForm;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ATStatusBar;

type
  TForm1 = class(TForm)
    bAdd: TButton;
    bDel: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure bDelClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
    procedure ItemClick(S: TObject; Index: Integer);
    procedure ItemDraw(Sender: TObject; AIndex: Integer;
      ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean);
  public
    { Public declarations }
    t, t0: TATStatus;
  end;

var
  Form1: TForm1;

implementation

uses StrUtils, Math;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  t:= TATStatus.Create(Self);
  t.Parent:= Self;
  t.IndentLeft:= 2;
  t.OnPanelClick:= ItemClick;
  t.OnPanelDrawAfter:= ItemDraw;

  t.AddPanel(100, taLeftJustify, 'Left');
  t.AddPanel(100, taCenter, 'Middle');
  t.AddPanel(150, taLeftJustify, 'Item wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww');
  t.AddPanel(150, taRightJustify, 'Right');

  //-----------------------------------
  t0:= TATStatus.Create(Self);
  t0.Parent:= Self;
  t0.IndentLeft:= 2;
  t0.OnPanelClick:= ItemClick;
  t0.Top:= ClientHeight-2;

  t0.Color:= clLtGray;
  t0.ColorBorderTop:= clWhite;
  t0.ColorBorderR:= clGray;
  t0.ColorBorderD:= clGray;
  t0.ColorBorderL:= clWhite;
  t0.ColorBorderU:= clNone;
  t0.Font.Color:= clNavy;
  t0.Font.Size:= 13;
  t0.Height:= 32;

  t0.AddPanel(50, taRightJustify, 'Rt');
  t0.AddPanel(50, taRightJustify, 'Rt2');
  t0.AddPanel(100, taCenter, 'Middle');
  t0.AddPanel(300, taLeftJustify, 'Item wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww');
end;

procedure TForm1.bAddClick(Sender: TObject);
begin
  t.AddPanel(50, taCenter, 'n'+StringOfChar('n', Random(8)));
end;

procedure TForm1.bDelClick(Sender: TObject);
begin
  t.DeletePanel(t.PanelCount-1);
end;

procedure TForm1.Edit1Change(Sender: TObject);
var
  d: TATStatusData;
begin
  if t.PanelCount<1 then Exit;
  d:= t.GetPanelData(0);
  d.ItemCaption:= Edit1.Text;
  t.Invalidate;
end;

procedure TForm1.ItemClick(S: TObject; Index: Integer);
begin
  Label1.Caption:= 'Clicked panel '+Inttostr(Index) + ' of '+
    IfThen(S=t, 'light', 'dark') + ' bar';
end;

procedure TForm1.ItemDraw(Sender: TObject; AIndex: Integer;
  ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean);
begin
  Exit;
  //test
  ACanvas.Pen.Color:= clRed;
  ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Left+10, ARect.Top+20);
  ACanDraw:= false;
end;

end.
