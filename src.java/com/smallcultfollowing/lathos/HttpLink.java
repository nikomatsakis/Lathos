package com.smallcultfollowing.lathos;

import java.net.MalformedURLException;
import java.net.URL;

public class HttpLink extends LogLink {
	private final String hostname;
	private final int port;
	
	public HttpLink(String hostname, int port) {
		this.hostname = hostname;
		this.port = port;
	}

	@Override
	public LList<LogId> createNode(String msg) {
		try {
			URL url = new URL("http", hostname, port, "/createNodes");
			
		} catch (MalformedURLException e) {
			throw new RuntimeException(e); // should be impossible.
		}
	}
	
	
}
