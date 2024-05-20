unit View.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Controller.Empresa,
  Controller.Usuario, Controller.Login, Controller.Produto, Controller.Pedido,
  Vcl.Buttons, Vcl.ExtCtrls;

type
  TFrmMain = class(TForm)
    MainMenu1: TMainMenu;
    Cadastro1: TMenuItem;
    Empresa1: TMenuItem;
    Produto1: TMenuItem;
    Pedido1: TMenuItem;
    Usurio1: TMenuItem;
    Panel1: TPanel;
    btnSair: TSpeedButton;
    btnEmpresa: TSpeedButton;
    btnUsuario: TSpeedButton;
    btnPedido: TSpeedButton;
    btnProduto: TSpeedButton;
    procedure Empresa1Click(Sender: TObject);
    procedure Usurio1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Produto1Click(Sender: TObject);
    procedure Pedido1Click(Sender: TObject);
    procedure btnEmpresaClick(Sender: TObject);
    procedure btnProdutoClick(Sender: TObject);
    procedure btnPedidoClick(Sender: TObject);
    procedure btnUsuarioClick(Sender: TObject);
    procedure btnSairClick(Sender: TObject);
  private
    { Private declarations }
    FEmpresa : TEmpresaController;
    FUsuario : TUsuarioController;
    FLogin   : TLoginController;
    FProduto : TProdutoController;
    FPedido  : TPedidoController;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

procedure TFrmMain.btnEmpresaClick(Sender: TObject);
begin
  Empresa1Click(Sender);
end;

procedure TFrmMain.btnPedidoClick(Sender: TObject);
begin
  Pedido1Click(Sender);
end;

procedure TFrmMain.btnProdutoClick(Sender: TObject);
begin
  Produto1Click(Sender);
end;

procedure TFrmMain.btnSairClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFrmMain.btnUsuarioClick(Sender: TObject);
begin
  Usurio1Click(Sender);
end;

procedure TFrmMain.Empresa1Click(Sender: TObject);
begin
  try
    Self.FEmpresa := TEmpresaController.Create;
  finally
    FreeAndNil(Self.FEmpresa);
  end;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  try
    Self.FLogin := TLoginController.Create;
  finally
    FreeAndNil(Self.FLogin);
  end;
end;

procedure TFrmMain.Pedido1Click(Sender: TObject);
begin
  try
    Self.FPedido := TPedidoController.Create;
  finally
    FreeAndNil(Self.FPedido);
  end;
end;

procedure TFrmMain.Produto1Click(Sender: TObject);
begin
  try
    Self.FProduto := TProdutoController.Create;
  finally
    FreeAndNil(Self.FProduto);
  end;
end;

procedure TFrmMain.Usurio1Click(Sender: TObject);
begin
  try
    Self.FUsuario := TUsuarioController.Create;
  finally
    FreeAndNil(Self.FUsuario);
  end;
end;

end.
