unit Model.Pedido;

interface

uses
  System.Classes;

type
  TPedidoModel = class
  private
    Fin_situacao: String;
    Fdt_alteracao: TDateTime;
    Fco_empresa: Integer;
    Fdt_cadastro: TDateTime;
    Fnu_pedido: Integer;
    Fvl_pedido: Currency;
    procedure Setco_empresa(const Value: Integer);
    procedure Setdt_alteracao(const Value: TDateTime);
    procedure Setdt_cadastro(const Value: TDateTime);
    procedure Setin_situacao(const Value: String);
    procedure Setnu_pedido(const Value: Integer);
    procedure Setvl_pedido(const Value: Currency);
  public
    property nu_pedido : Integer read Fnu_pedido write Setnu_pedido;
    property co_empresa : Integer read Fco_empresa write Setco_empresa;
    property in_situacao : String read Fin_situacao write Setin_situacao;
    property vl_pedido : Currency read Fvl_pedido write Setvl_pedido;
    property dt_cadastro : TDateTime read Fdt_cadastro write Setdt_cadastro;
    property dt_alteracao : TDateTime read Fdt_alteracao write Setdt_alteracao;
  end;

implementation

{ TPedidoController }

{ TPedidoModel }

procedure TPedidoModel.Setco_empresa(const Value: Integer);
begin
  Fco_empresa := Value;
end;

procedure TPedidoModel.Setdt_alteracao(const Value: TDateTime);
begin
  Fdt_alteracao := Value;
end;

procedure TPedidoModel.Setdt_cadastro(const Value: TDateTime);
begin
  Fdt_cadastro := Value;
end;

procedure TPedidoModel.Setin_situacao(const Value: String);
begin
  Fin_situacao := Value;
end;

procedure TPedidoModel.Setnu_pedido(const Value: Integer);
begin
  Fnu_pedido := Value;
end;

procedure TPedidoModel.Setvl_pedido(const Value: Currency);
begin
  Fvl_pedido := Value;
end;

end.
