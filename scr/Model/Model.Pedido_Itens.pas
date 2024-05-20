unit Model.Pedido_Itens;

interface

uses
  System.Classes;

type
  TPedido_ItensModel = class
  private
    Fco_produto: Integer;
    Fvl_total: Currency;
    Fnu_quantidade: Double;
    Fdt_cadastro: TDateTime;
    Fvl_venda: Currency;
    Fnu_pedido: Integer;
    Fco_item: Integer;
    procedure Setco_produto(const Value: Integer);
    procedure Setdt_cadastro(const Value: TDateTime);
    procedure Setnu_pedido(const Value: Integer);
    procedure Setnu_quantidade(const Value: Double);
    procedure Setvl_total(const Value: Currency);
    procedure Setvl_venda(const Value: Currency);
    procedure Setco_item(const Value: Integer);
  public
    property co_item : Integer read Fco_item write Setco_item;
    property nu_pedido : Integer read Fnu_pedido write Setnu_pedido;
    property co_produto : Integer read Fco_produto write Setco_produto;
    property nu_quantidade : Double read Fnu_quantidade write Setnu_quantidade;
    property vl_venda : Currency read Fvl_venda write Setvl_venda;
    property vl_total : Currency read Fvl_total write Setvl_total;
    property dt_cadastro : TDateTime read Fdt_cadastro write Setdt_cadastro;
  end;

implementation

{ TPedido_ItensModel }

procedure TPedido_ItensModel.Setco_item(const Value: Integer);
begin
  Fco_item := Value;
end;

procedure TPedido_ItensModel.Setco_produto(const Value: Integer);
begin
  Fco_produto := Value;
end;

procedure TPedido_ItensModel.Setdt_cadastro(const Value: TDateTime);
begin
  Fdt_cadastro := Value;
end;

procedure TPedido_ItensModel.Setnu_pedido(const Value: Integer);
begin
  Fnu_pedido := Value;
end;

procedure TPedido_ItensModel.Setnu_quantidade(const Value: Double);
begin
  Fnu_quantidade := Value;
end;

procedure TPedido_ItensModel.Setvl_total(const Value: Currency);
begin
  Fvl_total := Value;
end;

procedure TPedido_ItensModel.Setvl_venda(const Value: Currency);
begin
  Fvl_venda := Value;
end;

end.
