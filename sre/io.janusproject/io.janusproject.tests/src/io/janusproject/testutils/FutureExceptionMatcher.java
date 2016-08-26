/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.janusproject.testutils;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.hamcrest.Description;
import org.hamcrest.DiagnosingMatcher;

/**
 * A matcher that is testing if a future is throwing an exception.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class FutureExceptionMatcher extends DiagnosingMatcher<Future<?>> {

	private final Class<? extends Throwable> expected;

	/**
	 * @param expected - expected exception.
	 */
	public FutureExceptionMatcher(Class<? extends Throwable> expected) {
		this.expected = expected;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void describeTo(Description description) {
		description.appendText("an instance of ").appendText(this.expected.getName()); //$NON-NLS-1$
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected boolean matches(Object item, Description mismatch) {
		if (null == item) {
			mismatch.appendText("null"); //$NON-NLS-1$
			return false;
		}

		if (!(item instanceof Future<?>)) {
			mismatch.appendValue(item).appendText(" is not a Future"); //$NON-NLS-1$
			return false;
		}

		Future<?> f = (Future<?>) item;

		try {
			f.get(30, TimeUnit.SECONDS);
		} catch (Throwable e) {
			while (e instanceof ExecutionException) {
				e = e.getCause();
			}
			if (!this.expected.isInstance(e)) {
				mismatch.appendValue(e).appendText(" is a " + e.getClass().getName()); //$NON-NLS-1$
				return false;
			}
			return true;
		}

		mismatch.appendText("an exception of type " + this.expected + " is expected"); //$NON-NLS-1$ //$NON-NLS-2$
		return false;
	}

}
