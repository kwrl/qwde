package qwde.web.servlets;

import io.micronaut.http.HttpRequest;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.HttpStatus;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Error;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import qwde.web.plotly.PageRenderer;

import java.util.Collections;

@Controller(value = "/notfound", produces = MediaType.TEXT_HTML)
public class GenericNon200HttpHandler {
    private static final Logger LOG = LoggerFactory.getLogger(GenericNon200HttpHandler.class);

    @Error(status = HttpStatus.NOT_FOUND, global = true)
    public HttpResponse notFound(HttpRequest request) {
        LOG.debug("HTTP 404: {}", request.getUri());
        return HttpResponse.ok(PageRenderer.renderPage("notfound.ftl", Collections.singletonMap("pageTitle", "404 not found")))
                .contentType(MediaType.TEXT_HTML);
    }

    @Error(global = true)
    public HttpResponse error(HttpRequest request, Throwable e) {
        LOG.error("Something bad happened", e);
        return HttpResponse.ok(PageRenderer.renderPage("errorpage.ftl", Collections.singletonMap("pageTitle", "500 error")))
                .contentType(MediaType.TEXT_HTML);
    }
}
