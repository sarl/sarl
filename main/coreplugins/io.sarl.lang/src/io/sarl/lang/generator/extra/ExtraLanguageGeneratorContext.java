/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.generator.extra;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.generator.IFileSystemAccess2;
import org.eclipse.xtext.generator.IGeneratorContext;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/** The generator from SARL to the Python language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ExtraLanguageGeneratorContext implements IExtraLanguageGeneratorContext {

	private final UUID identifier;

	private final Date generationDate;

	private final IGeneratorContext delegate;

	private final WeakReference<IRootGenerator> rootGenerator;

	private final Resource resource;

	private IFileSystemAccess2 fileSystemAccess;

	private Map<String, Object> temporaryData;

	private LightweightTypeReference expectedExpressionType;

	/** Create the context for the given delegate.
	 *
	 * @param delegate the delegate.
	 * @param fileSystemAccess the file system access.
	 * @param generator the root generator.
	 * @param resource the resource.
	 */
	public ExtraLanguageGeneratorContext(IGeneratorContext delegate, IFileSystemAccess2 fileSystemAccess,
			IRootGenerator generator, Resource resource) {
		this.identifier = UUID.randomUUID();
		this.generationDate = new Date();
		this.delegate = delegate;
		this.fileSystemAccess = fileSystemAccess;
		this.resource = resource;
		this.rootGenerator = new WeakReference<>(generator);
	}

	@Override
	public UUID getGenerationID() {
		return this.identifier;
	}

	@Override
	public Date getGenerationDate() {
		return this.generationDate;
	}

	@Override
	public Resource getResource() {
		return this.resource;
	}

	@Override
	public IRootGenerator getRootGenerator() {
		return this.rootGenerator.get();
	}

	@Override
	public CancelIndicator getCancelIndicator() {
		final CancelIndicator indicator = this.delegate.getCancelIndicator();
		if (indicator == null) {
			return CancelIndicator.NullImpl;
		}
		return indicator;
	}

	@Override
	public IGeneratorContext getDelegate() {
		return this.delegate;
	}

	@Override
	public IFileSystemAccess2 getFileSystemAccess() {
		return this.fileSystemAccess;
	}

	@Override
	public <T> T getData(String id, Class<T> type, T defaultValue) {
		if (Strings.isEmpty(id) || this.temporaryData == null) {
			return defaultValue;
		}
		final Object data = this.temporaryData.get(id);
		if (data == null) {
			return defaultValue;
		}
		try {
			return type.cast(data);
		} catch (Throwable exception) {
			return defaultValue;
		}
	}

	@Override
	public <T> T getData(String id, Class<T> type) {
		return getData(id, type, null);
	}

	@Override
	public void setData(String id, Object value) {
		if (Strings.isEmpty(id)) {
			return;
		}
		if (value == null) {
			if (this.temporaryData != null) {
				this.temporaryData.remove(id);
			}
			return;
		}
		if (this.temporaryData == null) {
			this.temporaryData = new TreeMap<>();
		}
		this.temporaryData.put(id, value);
	}

	@Override
	public void clearData() {
		this.temporaryData = null;
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T> List<T> getListData(String id, Class<T> type) {
		List<T> list = null;
		if (!Strings.isEmpty(id) && this.temporaryData != null) {
			final Object obj = this.temporaryData.get(id);
			if (obj instanceof List) {
				list = (List<T>) obj;
			}
		}
		if (list == null) {
			list = new ArrayList<>();
			if (this.temporaryData == null) {
				this.temporaryData = new TreeMap<>();
			}
			this.temporaryData.put(id, list);
		}
		return list;
	}

	@Override
	public LightweightTypeReference getExpectedExpressionType() {
		return this.expectedExpressionType;
	}

	@Override
	public LightweightTypeReference setExpectedExpressionType(LightweightTypeReference expectedType) {
		final LightweightTypeReference old = this.expectedExpressionType;
		if (expectedType != null && expectedType.isPrimitiveVoid()) {
			this.expectedExpressionType = null;
		} else {
			this.expectedExpressionType = expectedType;
		}
		return old;
	}

}
