package qwde.dataprovider.kafka.serializer;

import org.apache.commons.lang3.SerializationUtils;
import org.apache.kafka.common.serialization.Deserializer;
import qwde.dataprovider.models.StockTicker;

public class StockTickerDeserializer implements Deserializer<StockTicker> {
    @Override
    public StockTicker deserialize(String topic, byte[] data) {
        return SerializationUtils.deserialize(data);
    }
}
