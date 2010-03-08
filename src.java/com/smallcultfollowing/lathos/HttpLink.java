package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.Charset;

public class HttpLink extends LogLink {
	private final String hostname;
	private final int port;
	private final LogParser parser = new LogParser();
	
	public HttpLink(String hostname, int port) {
		this.hostname = hostname;
		this.port = port;
	}
	
	@Override
	public LList<LogId> createNode(String msg) {
		try {
			URL url = new URL("http", hostname, port, "/createNodes");
			HttpURLConnection connection = (HttpURLConnection) url.openConnection();
			connection.setDoInput(true);
			connection.setDoOutput(true);
			
			OutputStream outputStream = connection.getOutputStream();
			PrintWriter writer = new PrintWriter(new OutputStreamWriter(outputStream, Charset.forName("UTF-8")));
			writer.println(msg);
			writer.close();
			
			// expected response: a list of ids like [ {id, "string", version}... ]
			InputStream inputStream = connection.getInputStream();
			Reader reader = new InputStreamReader(inputStream, Charset.forName("UTF-8"));
			return parser.parseStreamAsLogIdList(reader);
		} catch (MalformedURLException e) {
			throw new RuntimeException(e); // should be impossible.
		} catch (IOException e) {
			return null; // ignore connection errors... what you gonna' do?
		} catch (ParseFailure e) {
			System.err.printf("Parse failure: %s", e.toString());
			return null; // parse failure... should at least log it...
		}
	}
	
	
}
