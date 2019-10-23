package qwde.web.plotly;

import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Locale;
import java.util.Map;

public final class PageRenderer {
    private static final Logger LOG = LoggerFactory.getLogger(PageRenderer.class);
    private static final Configuration CFG = new Configuration(Configuration.VERSION_2_3_29);

    private PageRenderer() {
    }

    static {
        CFG.setClassForTemplateLoading(PageRenderer.class, "/");
        CFG.setDefaultEncoding("UTF-8");
        CFG.setLocale(Locale.US);
    }

    public static String renderPage(String apacheFreemarkerFile, Map<String, Object> vars) {
        try {
            Template template = CFG.getTemplate(apacheFreemarkerFile);
            StringWriter stringWriter = new StringWriter();
            template.process(vars, stringWriter);
            return stringWriter.toString();
        } catch (TemplateException | IOException exception) {
            LOG.error("", exception);
            return "error occured, see logs :(";
        }
    }
}
