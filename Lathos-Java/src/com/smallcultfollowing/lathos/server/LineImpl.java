package com.smallcultfollowing.lathos.server;

import java.io.IOException;
import java.util.LinkedList;

import com.smallcultfollowing.lathos.model.Line;
import com.smallcultfollowing.lathos.model.CustomOutput;
import com.smallcultfollowing.lathos.model.Output;
import com.smallcultfollowing.lathos.model.PageContent;

public class LineImpl implements PageContent, Line {

	private final LinkedList<Object> contents = new LinkedList<Object>();
	
	@Override
	public synchronized void renderInPage(Output out) throws IOException {
		for(Object content : contents) {
			if(content instanceof CustomOutput) {
				((CustomOutput) content).renderInLine(out);
			} else {
				out.outputText(content.toString());
			}
		}
		
		out.outputText("<br>");
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

}
