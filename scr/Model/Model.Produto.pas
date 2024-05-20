unit Model.Produto;

interface

uses
  System.Classes;

type
  TProdutoModel = class
  private
    Fco_produto: Integer;
    Fnu_estoque: Double;
    Ftx_descricao: String;
    Fdt_alteracao: TDateTime;
    Fdt_cadastro: TDateTime;
    Fvl_venda: Currency;
    procedure Setco_produto(const Value: Integer);
    procedure Setdt_alteracao(const Value: TDateTime);
    procedure Setdt_cadastro(const Value: TDateTime);
    procedure Setnu_estoque(const Value: Double);
    procedure Settx_descricao(const Value: String);
    procedure Setvl_venda(const Value: Currency);
  public
    property co_produto : Integer read Fco_produto write Setco_produto;
    property tx_descricao : String read Ftx_descricao write Settx_descricao;
    property vl_venda : Currency read Fvl_venda write Setvl_venda;
    property nu_estoque : Double read Fnu_estoque write Setnu_estoque;
    property dt_cadastro : TDateTime read Fdt_cadastro write Setdt_cadastro;
    property dt_alteracao : TDateTime read Fdt_alteracao write Setdt_alteracao;
  end;

implementation

{ TProdutoModel }

procedure TProdutoModel.Setco_produto(const Value: Integer);
begin
  Fco_produto := Value;
end;

procedure TProdutoModel.Setdt_alteracao(const Value: TDateTime);
begin
  Fdt_alteracao := Value;
end;

procedure TProdutoModel.Setdt_cadastro(const Value: TDateTime);
begin
  Fdt_cadastro := Value;
end;

procedure TProdutoModel.Setnu_estoque(const Value: Double);
begin
  Fnu_estoque := Value;
end;

procedure TProdutoModel.Settx_descricao(const Value: String);
begin
  Ftx_descricao := Value;
end;

procedure TProdutoModel.Setvl_venda(const Value: Currency);
begin
  Fvl_venda := Value;
end;

end.
