package qwde.web.servlets;

import com.google.common.truth.Truth;
import io.micronaut.http.HttpRequest;
import io.micronaut.http.client.RxHttpClient;
import io.micronaut.http.client.annotation.Client;
import io.micronaut.test.annotation.MicronautTest;
import org.junit.jupiter.api.Test;

import javax.inject.Inject;
import java.util.List;

@MicronautTest
class RandomTest {

    @Inject
    @Client("/")
    RxHttpClient client;

    @Test
    void randomNumbers__returns() {
        List randomNumbers = client.toBlocking().retrieve(HttpRequest.GET("/random"), List.class);

        Truth.assertThat(randomNumbers.size()).isAtLeast(10);
    }
}
