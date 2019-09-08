CREATE TABLE StockTicker (
  id INT PRIMARY KEY,
  close_price REAL NOT NULL,
  low_price REAL NOT NULL,
  high_price REAL NOT NULL,
  volume INTEGER NOT NULL,
  symbol varchar(20) NOT NULL,
  timestamp DATETIME NOT NULL
);
