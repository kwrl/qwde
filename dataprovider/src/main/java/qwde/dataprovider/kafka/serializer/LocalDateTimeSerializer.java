package qwde.dataprovider.kafka.serializer;

import org.apache.commons.lang3.SerializationUtils;
import org.apache.kafka.common.serialization.Serializer;

import java.time.LocalDateTime;

public class LocalDateTimeSerializer implements Serializer<LocalDateTime> {
    @Override
    public byte[] serialize(String topic, LocalDateTime data) {
        return SerializationUtils.serialize(data);
    }

}
