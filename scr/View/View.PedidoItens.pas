unit View.PedidoItens;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Stan.Async,
  FireDAC.DApt, Vcl.Grids, Vcl.DBGrids, Vcl.DBCtrls;

type
  TFrmPedidoItens = class(TForm)
    Panel1: TPanel;
    btnNovo: TBitBtn;
    btnEditar: TBitBtn;
    btnGravar: TBitBtn;
    btnCancelar: TBitBtn;
    btnApagar: TBitBtn;
    btnFechar: TBitBtn;
    btnVoltar: TBitBtn;
    Panel2: TPanel;
    FDMPedido_Itens: TFDMemTable;
    dtsPedido_Itens: TDataSource;
    FDQProdutos: TFDQuery;
    dtsProdutos: TDataSource;
    FDMPedido_ItensNU_PEDIDO: TIntegerField;
    FDMPedido_ItensCO_PRODUTO: TIntegerField;
    FDMPedido_ItensNU_QUANTIDADE: TFloatField;
    FDMPedido_ItensVL_VENDA: TCurrencyField;
    FDMPedido_ItensVL_TOTAL: TCurrencyField;
    FDMPedido_ItensDT_CADASTRO: TDateTimeField;
    Panel3: TPanel;
    gridBase: TDBGrid;
    Label4: TLabel;
    edtQuantidade: TEdit;
    comboProduto: TDBLookupComboBox;
    Label2: TLabel;
    edtValorVenda: TEdit;
    Label3: TLabel;
    edtValorTotal: TEdit;
    edtCodigoProduto: TEdit;
    Label5: TLabel;
    FDMPedido_ItensCO_ITEM: TIntegerField;
    Label1: TLabel;
    edtCodigoItem: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

initialization
  RegisterClass(TFrmPedidoItens);

Finalization
  unRegisterClass(TFrmPedidoItens);

end.
