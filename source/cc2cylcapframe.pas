unit cc2CylCapFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ccCylPlaneCapFrame;

type
  TTwoCylCapFrame = class(TCylPlaneCapFrame)
  private
    { private declarations }
  protected
    function DoCalc(d, R, L, eps: Extended): Extended; override;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

uses
  ccGlobal;

constructor TTwoCylCapFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIniKey := '2 cylinders';
end;

function TTwoCylCapFrame.DoCalc(d, R, L, eps: Extended): Extended;
begin
  Result := PI * eps0 * eps * L / ln( (d/2 + sqrt(d*d*0.25 - R*R))/R);
end;

end.

