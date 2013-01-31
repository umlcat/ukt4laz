unit uktaccolls;

interface
uses
  Classes, 
  ActnList,
  dummy;

type

{ TSDVActionItem }

  TSDVActionItem = class(TCollectionItem)
  private
    { Private declarations }
  protected
    { Protected declarations }

    FAction: TBasicAction;
  public
    { Public declarations }
  published
    { Published declarations }

    property Action: TBasicAction
      read FAction write FAction;
  end;

{ TSDVActionCollection }

  TSDVActionCollection = class(TOwnedCollection)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Friend declarations }
  end;

implementation

end.
