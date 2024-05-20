unit Model.Imagens;

interface

uses
  System.Classes, Vcl.ExtCtrls, Vcl.Graphics;

type
  TImagensModel = class
  private
    Fco_produto: Integer;
    Fco_imagem: Integer;
    Ftx_extensao: String;
    Fdt_cadastro: TDateTime;
    Fim_produto: TStream;
    procedure Setco_imagem(const Value: Integer);
    procedure Setco_produto(const Value: Integer);
    procedure Setdt_cadastro(const Value: TDateTime);
    procedure Setim_produto(const Value: TStream);
    procedure Settx_extensao(const Value: String);
  public
    property co_imagem : Integer read Fco_imagem write Setco_imagem;
    property co_produto : Integer read Fco_produto write Setco_produto;
    property im_produto : TStream read Fim_produto write Setim_produto;
    property tx_extensao : String read Ftx_extensao write Settx_extensao;
    property dt_cadastro : TDateTime read Fdt_cadastro write Setdt_cadastro;
  end;

implementation

{ TImagensModel }

procedure TImagensModel.Setco_imagem(const Value: Integer);
begin
  Fco_imagem := Value;
end;

procedure TImagensModel.Setco_produto(const Value: Integer);
begin
  Fco_produto := Value;
end;

procedure TImagensModel.Setdt_cadastro(const Value: TDateTime);
begin
  Fdt_cadastro := Value;
end;

procedure TImagensModel.Setim_produto(const Value: TStream);
begin
  Fim_produto := Value;
end;

procedure TImagensModel.Settx_extensao(const Value: String);
begin
  Ftx_extensao := Value;
end;

end.
