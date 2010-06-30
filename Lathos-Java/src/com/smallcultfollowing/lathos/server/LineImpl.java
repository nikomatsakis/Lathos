package com.smallcultfollowing.lathos.server;

import java.io.IOException;
import java.util.LinkedList;

import com.smallcultfollowing.lathos.model.Line;
import com.smallcultfollowing.lathos.model.CustomOutput;
import com.smallcultfollowing.lathos.model.Output;
import com.smallcultfollowing.lathos.model.PageContent;

public class LineImpl implements PageContent, Line {

	private final LinkedList<Object> contents = new LinkedList<Object>();
	
	private final boolean bolded;
	
	public LineImpl() {
		this.bolded = false;
	}
	
	public LineImpl(boolean bolded) {
		this.bolded = bolded;
	}
	
	@Override
	public synchronized void renderInPage(Output out) throws IOException {
		out.startLine();
		
		if(bolded) {
			out.startBold();
		}
		
		for(Object content : contents) {
			if(content instanceof CustomOutput) {
				((CustomOutput) content).renderInLine(out);
			} else {
				out.outputText(content.toString());
			}
		}
		
		if(bolded) {
			out.endBold();
		}
		
		out.endLine();
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
