package qwde.dataprovider.kafka.serializer;

import org.apache.commons.lang3.SerializationUtils;
import org.apache.kafka.common.serialization.Serializer;
import qwde.dataprovider.models.StockTicker;

public class StockTickerSerializer implements Serializer<StockTicker> {
    @Override
    public byte[] serialize(String topic, StockTicker data) {
        return SerializationUtils.serialize(data);
    }
}
