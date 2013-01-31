unit udmDataModule;

interface

uses
  SysUtils, Classes, ImgList, Controls;

type
  TdmDataModule = class(TDataModule)
    imlsItems: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmDataModule: TdmDataModule;

implementation

{$R *.dfm}

end.
