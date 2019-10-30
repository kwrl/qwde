package qwde.web.servlets;

import io.micronaut.http.HttpResponse;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;

import java.util.Collection;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;

@Controller(value = "/random2", produces = MediaType.APPLICATION_JSON)
public final class Random2 {

    @Get("/")
    public HttpResponse<Collection<Double>> doGet() {
        return HttpResponse.ok(DoubleStream
                .generate(ThreadLocalRandom.current()::nextDouble)
                .limit(100)
                .boxed()
                .collect(Collectors.toList()));
    }
}
