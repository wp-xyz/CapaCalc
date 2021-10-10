unit ccParallelCapFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ccManyCapFrame;

type
  TParallelCapFrame = class(TManyCapFrame)
  private

  public
    constructor Create(AOwner: TComponent); override;
    procedure Calculate; override;

  end;

implementation

{$R *.lfm}

constructor TParallelCapFrame.Create(AOwner: TComponent);
begin
  inherited;
  FIniKey := 'Parallel';
end;

procedure TParallelCapFrame.Calculate;
var
  capa, c : double;
  i: Integer;
begin
  try
    capa := 0.0;
    for i:=1 to Length(FEditArray) do
      if (FEditArray[i].Text <> '') and TryStrToFloat(FEditArray[i].Text, c) then
        capa := capa + c;
    if capa <> 0.0 then
      TxtCapa.Text := Format('%.*f', [FDecimals, capa])
    else
      TxtCapa.Clear;
  except
    TxtCapa.Clear;
  end;
end;

end.

