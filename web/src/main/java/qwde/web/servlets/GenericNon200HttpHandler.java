package qwde.web.servlets;

import io.micronaut.http.HttpRequest;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.HttpStatus;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Error;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Controller(value = "/notfound2", produces = MediaType.APPLICATION_JSON)
public class GenericNon200HttpHandler {
    private static final Logger LOG = LoggerFactory.getLogger(GenericNon200HttpHandler.class);

    @Error(status = HttpStatus.NOT_FOUND, global = true)
    public HttpResponse notFound(HttpRequest request) {
        LOG.debug("HTTP 404: {}", request.getUri());
        return HttpResponse.notFound("not found!");
    }

    @Error(global = true)
    public HttpResponse error(HttpRequest request, Throwable e) {
        LOG.error("Something bad happened", e);
        return HttpResponse.serverError("500!");
    }
}