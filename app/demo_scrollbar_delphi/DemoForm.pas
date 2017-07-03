unit DemoForm;

interface

uses
  {$ifndef fpc}
  Windows, Messages,
  {$endif}
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ATScrollBar, ComCtrls;

type
  TFormDemo = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    chkDraw: TCheckBox;
    trackBor: TTrackBar;
    Label1: TLabel;
    labv: TLabel;
    labh: TLabel;
    Label2: TLabel;
    trackPage: TTrackBar;
    Label3: TLabel;
    trackSize: TTrackBar;
    Label4: TLabel;
    trackLonger: TTrackBar;
    Label5: TLabel;
    trackCornerV: TTrackBar;
    trackCornerH: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure chkDrawClick(Sender: TObject);
    procedure trackBorChange(Sender: TObject);
    procedure trackPageChange(Sender: TObject);
    procedure trackSizeChange(Sender: TObject);
    procedure trackLongerChange(Sender: TObject);
    procedure trackCornerVChange(Sender: TObject);
    procedure trackCornerHChange(Sender: TObject);
  private
    { Private declarations }
    procedure ChangeH(S: TObject);
    procedure ChangeV(S: TObject);
    procedure DrawEvent(S: TObject; AType: TATScrollElemType;
      ACanvas: TCanvas; const ARect: TRect; var ACanDo: boolean);
  public
    { Public declarations }
    bh, bv, bbv, bbh: TATScroll;
  end;

var
  FormDemo: TFormDemo;

implementation

uses StrUtils, Math;

{$R *.dfm}

procedure TFormDemo.FormCreate(Sender: TObject);
begin
  bh:= TATScroll.Create(Self);
  bh.Parent:= Panel1;
  bh.Align:= alBottom;
  bh.Kind:= sbHorizontal;
  bh.OnChange:= {$ifdef fpc}@{$endif} ChangeH;
  bh.Min:= 20;
  bh.Max:= 200;

  //-----------------------------------
  bv:= TATScroll.Create(Self);
  bv.Parent:= Panel1;
  bv.Align:= alRight;
  bv.Kind:= sbVertical;
  bv.OnChange:= {$ifdef fpc}@{$endif} ChangeV;
  bv.Min:= 10;
  bv.Max:= 100;

  bv.Width:= 22;
  bh.Height:= bv.Width;
  bh.IndentCorner:= bv.Width; //positive

  //--------------
  bbh:= TATScroll.Create(Self);
  bbh.Parent:= Panel2;
  bbh.Height:= 18;
  bbh.Align:= alBottom;
  bbh.Kind:= sbHorizontal;
  bbh.IndentCorner:= -bbh.Height; //negative

  bbv:= TATScroll.Create(Self);
  bbv.Parent:= Panel2;
  bbv.Width:= bbh.Height;
  bbv.Align:= alLeft;
  bbv.Kind:= sbVertical;
end;

procedure TFormDemo.DrawEvent(S: TObject; AType: TATScrollElemType;
  ACanvas: TCanvas; const ARect: TRect; var ACanDo: boolean);
const
  cc: array[TATScrollElemType] of TColor = (
    clYellow, clYellow,
    clCream, clCream,
    $30a030, $00e000, 
    $e0a0c0, clNavy,
    clRed div 2, clRed,
    $e05050);
begin
  ACanvas.Brush.Color:= cc[AType];
  ACanvas.FillRect(ARect);
  ACanDo:= false;
end;

procedure TFormDemo.chkDrawClick(Sender: TObject);
begin
  if chkDraw.Checked then
  begin
    bh.OnOwnerDraw:= {$ifdef fpc}@{$endif} DrawEvent;
    bv.OnOwnerDraw:= {$ifdef fpc}@{$endif} DrawEvent;
  end
  else
  begin
    bh.OnOwnerDraw:= nil;
    bv.OnOwnerDraw:= nil;
  end;
  bh.Invalidate;
  bv.Invalidate;
end;

procedure TFormDemo.trackBorChange(Sender: TObject);
begin
  bv.IndentBorder:= trackBor.Position;
  bh.IndentBorder:= bv.IndentBorder;
  bv.Invalidate;
  bh.Invalidate;
end;

procedure TFormDemo.ChangeH(S: TObject);
begin
  labh.Caption:= Format('Horz %d (%d .. %d)', [bh.Position, bh.Min, bh.Max]);
end;

procedure TFormDemo.ChangeV(S: TObject);
begin
  labv.Caption:= Format('Vert %d (%d .. %d)', [bv.Position, bv.Min, bv.Max]);
end;

procedure TFormDemo.trackPageChange(Sender: TObject);
begin
  bv.PageSize:= trackPage.Position;
  bh.PageSize:= bv.PageSize;
end;

procedure TFormDemo.trackSizeChange(Sender: TObject);
var
  n: Integer;
begin
  n:= trackSize.Position;
  bv.IndentArrow:= n div 5;
  bh.IndentArrow:= bv.IndentArrow;
  bv.Width:= n;
  bh.IndentCorner:= n;
  bh.Height:= n;
end;

procedure TFormDemo.trackLongerChange(Sender: TObject);
begin
  bh.IndentArrLonger:= trackLonger.Position;
  bv.IndentArrLonger:= trackLonger.Position;
  bh.Invalidate;
  bv.Invalidate;
end;

procedure TFormDemo.trackCornerVChange(Sender: TObject);
begin
  bv.IndentCorner:= trackCornerV.Position;
  bv.Invalidate;
end;

procedure TFormDemo.trackCornerHChange(Sender: TObject);
begin
  bh.IndentCorner:= trackCornerH.Position;
  bh.Invalidate;
end;

end.
