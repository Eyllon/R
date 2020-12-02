# otimizacao.R
O programa busca otimizar portfólios a partir de diferentes medidas de risco. O portfólio analisado foi contruído a partir das ações recomendadas pela xp em agosto de 2019. Os dados são
retirado do Yahoofinance utilizando o comando getSymbols do pacote quantmod. Os missings na base de dados foram tratados via interpolação linar. Vale notar que uma forma alternativa e 
mais adequada é utilizar o modelo de volatilidade estocástica para interpolar.

A principal motivação para a análise é avaliar o desempenho do modelo GARCH com Mudança de Regima (MS-GARCH) com relação ao GARCH(1,1) tradicional. A ideia é que o MSGARCH permite tro-
cas no modelo gerador da volatilidade ao longo do processo de estimação/previsão. No cenário atual identificamos constantes choques no mercado e uma estrutar mais "flexível" captura e
incorpora estes choques de forma mais eficáz do que modelos mais simples. O benchmark de nossa análise foi a estimação padrão do PortfólioAnalytics que utiliza desvio padrão como medida
de risco. Incluí também o modelo GJR-GARCH (que também permite certa flexibilidade no comportamento da volatilidade) e o modelo Volatilidade Estocástica que é uma estrutura mais robusta.

A dificuldade de implementar tanto o modelo MS-GARCH quanto o modelo SV encontra-se na estimação. O método de Markov Chain Monte Carlo é o mais adequado para os dois modelos, mas é com-
putacionalmente muito mais exigente do que a verossimilhâça. Adicionalmente me deparei com dificuldades de conhecimento teórico na implementação do método, o que me levou a evitar cus-
tomizar a estrutura básica da função de MCMC e a utilizar a versão mais simples do SV. Futuramente voltarei com essa análise selecionando o tipo de filtro, pontos de partida e estrutura
mais adequados para os modelos.

Por fim faço algumas considerações sobre o resultado. Apesar das dificuldades técnicas na implementação, O MS-GARCH se mostrou o modelo superior. O portfólio gerado a partir das estima-
tivas apresentou o maior retorno acumulado e o maior índice Sharpe (que em todos os casos, foi bastante baixo). Apesar de apresentar desempenho inferior, o modelo GARCH apresenta uma 
vantagem sobre o MS-GARCH e SV que deve ser destacado. O pacote rugarch é o mais simples de implementar em conjunto com o PortfólioAnalytics e o pacote em si apresenta diversas funções
que permitem análises profundas sobre o risco do portfólio. Dentre elas destaco o ugarchroll utilizado no código VaR que permite projetar um VaR dinâmico de maneira automática. Imple-
mentar uma análise semelhante utilizando MS-GARCH e SV exige um código mais complexo e muito mais poder computacional.
