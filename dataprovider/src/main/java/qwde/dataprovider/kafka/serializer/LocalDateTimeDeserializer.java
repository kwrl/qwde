package qwde.dataprovider.kafka.serializer;

import org.apache.commons.lang3.SerializationUtils;
import org.apache.kafka.common.serialization.Deserializer;

import java.time.LocalDateTime;

public class LocalDateTimeDeserializer implements Deserializer<LocalDateTime> {
    @Override
    public LocalDateTime deserialize(String topic, byte[] data) {
        return SerializationUtils.deserialize(data);
    }
}
