unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ATFlatThemes,
  ATListbox;

type

  { TForm1 }

  TForm1 = class(TForm)
    List: TATListbox;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  Cols: TATIntArray;
begin
  SetLength(Cols, 4);
  Cols[0]:= -40; //percents
  Cols[1]:= 0; //autosize
  Cols[2]:= 0; //autosize
  Cols[3]:= 50; //pixels

  List.VirtualMode:= false; //must have for columns
  List.ColumnSizes:= Cols;
  List.ColumnSeparator:= '|';

  List.Items.Add('aaaaaaaaaaaa1|aa2|aaaaaaaaaaaaaaaaaaaaaaaaaaaa3|aa4|aaaaaaaaaaaaaaaaaaaaaaaaaaaa5');
  List.Items.Add('bb1|bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb2');
  List.Items.Add('dddddddddddddddddddddddddddddddddddddddd1|dd2|dddddddddddddddddddddddddddd3|dddddddddddd4');
  List.Items.Add('ee1|||ee4');

  List.Invalidate;
end;

end.

