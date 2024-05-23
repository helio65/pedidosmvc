program SysPedidos;

uses
  Vcl.Forms,
  View.Main in 'View\View.Main.pas' {FrmMain},
  Model.Empresa in 'Model\Model.Empresa.pas',
  View.Empresa in 'View\View.Empresa.pas' {FrmEmpresa},
  Controller.Empresa in 'Controller\Controller.Empresa.pas',
  Framework.Factory in 'Framework\Framework.Factory.pas',
  Dao.Conexao in 'Dao\Dao.Conexao.pas' {DMConexao: TDataModule},
  Model.Usuario in 'Model\Model.Usuario.pas',
  View.Usuario in 'View\View.Usuario.pas' {FrmUsuario},
  Controller.Usuario in 'Controller\Controller.Usuario.pas',
  View.Login in 'View\View.Login.pas' {FrmLogin},
  Controller.Login in 'Controller\Controller.Login.pas',
  View.Produto in 'View\View.Produto.pas' {FrmProduto},
  Model.Produto in 'Model\Model.Produto.pas',
  Controller.Produto in 'Controller\Controller.Produto.pas',
  View.Imagens in 'View\View.Imagens.pas' {FrmImagem},
  Model.Imagens in 'Model\Model.Imagens.pas',
  Controller.Imagens in 'Controller\Controller.Imagens.pas',
  Model.Pedido in 'Model\Model.Pedido.pas',
  View.Pedido in 'View\View.Pedido.pas' {FrmPedido},
  Controller.Pedido in 'Controller\Controller.Pedido.pas',
  View.PedidoItens in 'View\View.PedidoItens.pas' {FrmPedidoItens},
  Model.PedidoItens in 'Model\Model.PedidoItens.pas',
  Controller.PedidoItens in 'Controller\Controller.PedidoItens.pas',
  Dao.Usuario in 'Dao\Dao.Usuario.pas',
  Dao.Empresa in 'Dao\Dao.Empresa.pas',
  Dao.Produto in 'Dao\Dao.Produto.pas',
  Dao.Imagens in 'Dao\Dao.Imagens.pas',
  Dao.Pedido in 'Dao\Dao.Pedido.pas',
  Dao.PedidoItens in 'Dao\Dao.PedidoItens.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown   := True;
  Application.CreateForm(TDMConexao, DMConexao);
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
