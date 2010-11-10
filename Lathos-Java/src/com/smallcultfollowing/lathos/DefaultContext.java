package com.smallcultfollowing.lathos;

import java.util.LinkedList;
import java.util.List;



public class DefaultContext implements Context {
	
	private final LathosServer server;
	private final LinkedList<Page> stack = new LinkedList<Page>();
	private Line previousLine;
	
	public DefaultContext(LathosServer server, List<Page> initialStack) {
		this.server = server;
		stack.addAll(initialStack);
	}
	
	private void addToLine(Line line, Object... objects) {
		for(Object o : objects) 
			server.addToLine(line, o);
	}

	@Override
	public Page pushTopLevel(String id, Object... title) {
		Page page = Lathos.topLevelPage(server, id, title);
		return push(page);
	}
	
	@Override
	public void pushDisabledPage() {
		push(NonePage.Page);
	}
	
	@Override
	public Page pushLinkedChild(String id, Object... title) {
		if(stack.isEmpty())
			return pushTopLevel(id, title);
		Page topPage = stack.getLast();
		Page subPage = Lathos.newPage(server, topPage, id, title);
		log(link(subPage, "\u25B2"));
		append(title);
		return push(subPage);
	}

	@Override
	public Page pushEmbeddedChild(String id, Object... title) {
		if(stack.isEmpty())
			return pushTopLevel(id, title);
		Page topPage = stack.getLast();
		Page subPage = Lathos.newPage(server, topPage, id, title);
		topPage.addContent(subPage);
		return push(subPage);
	}
	
	@Override
	public Page push(Page page) {
		stack.add(page);
		return page;
	}

	@Override
	public void embedIn(Page page) {
		Page topPage = stack.getLast();
		if(topPage != NonePage.Page) {
			page.addContent(topPage);
		}
	}

	@Override
	public Line log(Object... contents) {
		previousLine = Lathos.para(server, stack.getLast(), contents);
		return previousLine;
	}

	@Override
	public void append(Object... contents) {
		addToLine(previousLine, contents);
	}

	@Override
	public void append(Line line, Object... contents) {
		addToLine(line, contents);
	}

	@Override
	public void pop(Page page) {
		if(page != null) {
			if(stack.getLast() != page)
				throw new RuntimeException(
						String.format(
								"Popping %s but passed in %s", 
								stack.getLast(),
								page));
		}
		
		stack.removeLast();
	}

	@Override
	public LathosServer server() {
		return server;
	}

	@Override
	public Context context() {
		return new DefaultContext(server, stack);
	}

	@Override
	public CustomOutput link(Page page, Object... content) {
		Link result = new Link(page);
		addToLine(result, content);
		return result;
	}

	@Override
	public Page topPage() {
		return stack.getLast();
	}

}
