package qwde.web.servlets;

import com.google.common.truth.Truth;
import io.micronaut.core.type.Argument;
import io.micronaut.http.HttpRequest;
import io.micronaut.http.client.RxHttpClient;
import io.micronaut.http.client.annotation.Client;
import io.micronaut.test.annotation.MicronautTest;
import org.junit.jupiter.api.Test;

import javax.inject.Inject;
import java.util.List;
import java.util.Map;

@MicronautTest
class RandomTest {

    @Inject
    @Client("/")
    RxHttpClient client;

    @Test
    void randomNumbers__returns() {
        var resp = client.toBlocking().retrieve(HttpRequest.GET("/random"), Argument.of(Map.class, String.class, List.class));
        @SuppressWarnings("unchecked")
        List<Double> numbers = (List<Double>) resp.get("numbers");

        Truth.assertThat(numbers.size()).isAtLeast(10);
    }
}
