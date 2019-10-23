package qwde.dataprovider.models;

import java.io.Serializable;
import java.time.LocalDateTime;

public interface IStockTicker extends Comparable<IStockTicker>, Serializable {
    double getPrice();

    double getHigh();

    double getLow();

    Long getVolume();

    String getCompany();

    LocalDateTime getTimestamp();
}
