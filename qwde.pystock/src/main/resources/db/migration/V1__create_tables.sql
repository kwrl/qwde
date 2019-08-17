CREATE TABLE StockTicker (
  id INT PRIMARY KEY,
  price REAL NOT NULL,
  symbol varchar(20) NOT NULL,
  timestamp DATETIME NOT NULL
);
