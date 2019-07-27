package qwde.servlets;

import java.io.IOException;
import java.util.Enumeration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class SharkToothServlet extends HttpServlet {
  private static Logger logger = LoggerFactory.getLogger(SharkToothServlet.class);
  private static final long serialVersionUID = 1L;

  protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

    Enumeration<String> parameterNames = request.getParameterNames();

    while (parameterNames.hasMoreElements()) {

      String paramName = parameterNames.nextElement();
      System.out.println(paramName);
      System.out.println(paramName);
      System.out.println(paramName);
      String[] paramValues = request.getParameterValues(paramName);
      for (int i = 0; i < paramValues.length; i++) {
        System.out.println(paramValues[i]);
        System.out.println(paramValues[i]);
        System.out.println(paramValues[i]);
      }
    }

    response.setContentType("text/html");
    response.setStatus(HttpServletResponse.SC_OK);
    response.getWriter().println("<h1>New Hello Shark rawr </h1>"); 
  } 
}

