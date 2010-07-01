package com.smallcultfollowing.lathos.model;

import java.io.IOException;
import java.util.LinkedList;


public class LineImpl implements CustomOutput, Line {

	private final LinkedList<Object> contents = new LinkedList<Object>();

	public LineImpl() {
		super();
	}

	@Override
	public synchronized void addContent(CustomOutput content) {
		contents.add(content);
	}

	@Override
	public synchronized void addText(String text) {
		contents.add(text);
	}

	@Override
	public synchronized void addNumber(Number num) {
		contents.add(num);
	}

	@Override
	public void renderInLine(Output output) throws IOException {
		for(Object content : contents) {
			if(content instanceof CustomOutput) {
				((CustomOutput) content).renderInLine(output);
			} else {
				output.outputText(content.toString());
			}
		}
	}

}