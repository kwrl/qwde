package qwde.web.servlets.configuration;

import io.micronaut.http.client.HttpClientConfiguration;
import io.micronaut.runtime.ApplicationConfiguration;

import javax.inject.Named;
import javax.inject.Singleton;
import java.time.Duration;

@Named("tradeengine")
@Singleton
public final class TradeEngineConfiguration extends HttpClientConfiguration {
    public TradeEngineConfiguration(ApplicationConfiguration applicationConfiguration) {
        super(applicationConfiguration);
        super.setReadTimeout(Duration.ofSeconds(100));
    }

    @Override
    public ConnectionPoolConfiguration getConnectionPoolConfiguration() {
        return new ConnectionPoolConfiguration();
    }
}
