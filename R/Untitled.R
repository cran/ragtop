# Use a quartic equation to generate some stock prices
generate.stock.prices = function(min_x, max_x, aa, bb, cc, dd, ee, n=25)
{
  x = seq(from=min_x, to=max_x, length.out=n)
  s = aa + bb*x + cc*x^2 + dd*x^3 + ee*x^4
  s
}

# Find returns for a given set of stock prices
find.stock.returns = function(stock_prices, num.prices=25)
{
  num.returns = num.prices - 1
  returns = rep(0, num.returns)
  for (i in 1:num.returns) {
    this.price = stock_prices[[i]]
    next.price = stock_prices[[i+1]]
    r = log(next.price/this.price)
    returns[[i]] = r
  }
  returns
}

# Use a quartic equation to generate some stock prices and find their returns
generate.stock.returns = function(min_x, max_x, aa, bb, cc=0, dd=0, ee=0, n=25)
{
  stock_prices = generate.stock.prices(min_x, max_x, aa, bb, cc, dd, ee)
  returns = find.stock.returns(stock_prices, num.prices=n)
  returns
}

if (F) {
# Usage examples, some of which show bugs
generate.stock.returns(0, 2, 1, 0.053331, -0.003)
generate.stock.returns(0, 2, 0.5, 0.14, -0.003, 0.01, 0.0005, 0.001)
generate.stock.returns(0, 2, 0.01, 0.04, -0.003, 0.01, n=15)
generate.stock.returns(0, 2, 0.01, 0.0003, -0.2, n=11)
generate.stock.returns(0, 2, 10.5, 0.03, -0.003, 0.08, n=50)
}
