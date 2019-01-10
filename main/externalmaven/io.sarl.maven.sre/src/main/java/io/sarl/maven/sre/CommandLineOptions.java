/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.maven.sre;

import java.util.Objects;

import org.apache.maven.plugins.annotations.Parameter;

/** Description of the command line options for the SRE.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class CommandLineOptions {

	@Parameter
	private String hideLogo;

	@Parameter
	private String showLogo;

	@Parameter
	private String hideInfo;

	@Parameter
	private String showInfo;

	@Parameter
	private String defaultContextId;

	@Parameter
	private String randomContextId;

	@Parameter
	private String bootAgentContextId;

	@Parameter
	private String offline;

	@Parameter
	private String embedded;

	@Parameter
	private String noMoreOption = "--"; //$NON-NLS-1$

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (obj.getClass().equals(getClass())) {
			final CommandLineOptions other = (CommandLineOptions) obj;
			return Objects.equals(getHideLogo(), other.getHideLogo())
				&& Objects.equals(getHideInfo(), other.getHideInfo())
				&& Objects.equals(getShowInfo(), other.getShowInfo())
				&& Objects.equals(getOffline(), other.getOffline())
				&& Objects.equals(getDefaultContextId(), other.getDefaultContextId())
				&& Objects.equals(getRandomContextId(), other.getRandomContextId())
				&& Objects.equals(getBootAgentContextId(), other.getBootAgentContextId())
				&& Objects.equals(getNoMoreOption(), other.getNoMoreOption())
				&& Objects.equals(getEmbedded(), other.getEmbedded());
		}
		return false;
	}

	@Override
	public int hashCode() {
		return Objects.hash(this.bootAgentContextId, this.defaultContextId, this.hideInfo, this.hideLogo,
				this.noMoreOption, this.offline, this.randomContextId, this.showInfo, this.embedded);
	}

	/** Replies the command line option for hiding the SRE logo.
	 *
	 * @return the command line option.
	 */
	public String getHideLogo() {
		return this.hideLogo;
	}


	/** Change the command line option for hiding the SRE logo.
	 *
	 * @param hideLogoCLIOption the command line option.
	 */
	public void setHideLogo(String hideLogoCLIOption) {
		this.hideLogo = hideLogoCLIOption;
	}

	/** Replies the command line option for showing the SRE logo.
	 *
	 * @return the command line option.
	 */
	public String getShowLogo() {
		return this.showLogo;
	}


	/** Change the command line option for showing the SRE logo.
	 *
	 * @param showLogoCLIOption the command line option.
	 */
	public void setShowLogo(String showLogoCLIOption) {
		this.showLogo = showLogoCLIOption;
	}

	/** Replies the command line option for hiding the SRE information messages.
	 *
	 * @return the command line option.
	 */
	public String getHideInfo() {
		return this.hideInfo;
	}


	/** Change the command line option for hiding the SRE information messages.
	 *
	 * @param hideInfoCLIOption the command line option.
	 */
	public void setHideInfo(String hideInfoCLIOption) {
		this.hideInfo = hideInfoCLIOption;
	}


	/** Replies the command line option for showing the SRE information messages.
	 *
	 * @return the command line option.
	 */
	public String getShowInfo() {
		return this.showInfo;
	}


	/** Change the command line option for showing the SRE information messages.
	 *
	 * @param showInfoCLIOption the command line option.
	 */
	public void setShowInfo(String showInfoCLIOption) {
		this.showInfo = showInfoCLIOption;
	}


	/** Replies the command line option for using the default identifier for the default context.
	 *
	 * @return the command line option.
	 */
	public String getDefaultContextId() {
		return this.defaultContextId;
	}


	/** Change the command line option for using the default identifier for the default context.
	 *
	 * @param defaultContextIdCLIOption the command line option.
	 */
	public void setDefaultContextId(String defaultContextIdCLIOption) {
		this.defaultContextId = defaultContextIdCLIOption;
	}


	/** Replies the command line option for using a random identifier for the default context.
	 *
	 * @return the command line option.
	 */
	public String getRandomContextId() {
		return this.randomContextId;
	}


	/** Change the command line option for using a random identifier for the default context.
	 *
	 * @param randomContextIdCLIOption the command line option.
	 */
	public void setRandomContextId(String randomContextIdCLIOption) {
		this.randomContextId = randomContextIdCLIOption;
	}


	/** Replies the command line option for using a identifier for the default context that is based on the boot agent identifier.
	 *
	 * @return the command line option.
	 */
	public String getBootAgentContextId() {
		return this.bootAgentContextId;
	}


	/** Change the command line option for using a identifier for the default context that is based on the boot agent identifier.
	 *
	 * @param bootAgentContextIdCLIOption the command line option.
	 */
	public void setBootAgentContextId(String bootAgentContextIdCLIOption) {
		this.bootAgentContextId = bootAgentContextIdCLIOption;
	}


	/** Replies the command line option for turning the network off.
	 *
	 * @return the command line option.
	 */
	public String getOffline() {
		return this.offline;
	}


	/** Change the command line option for turning the network off.
	 *
	 * @param offlineCLIOption the command line option.
	 */
	public void setOffline(String offlineCLIOption) {
		this.offline = offlineCLIOption;
	}


	/** Replies the command line option for indicates that no more option will be present on the rest of the command line.
	 *
	 * @return the command line option.
	 */
	public String getNoMoreOption() {
		return this.noMoreOption;
	}

	/** Change the command line option for indicates that no more option will be present on the rest of the command line.
	 *
	 * @param noMoreOptionCLIOption the command line option.
	 */
	public void setNoMoreOption(String noMoreOptionCLIOption) {
		this.noMoreOption = noMoreOptionCLIOption;
	}

	/** Replies the command line option for indicating that the SRE is embedded in another application.
	 *
	 * @return the command line option.
	 */
	public String getEmbedded() {
		return this.embedded;
	}

	/** Change the command line option for indicating that the SRE is embedded in another application.
	 *
	 * @param embeddedCLIOption the command line option.
	 */
	public void setEmbedded(String embeddedCLIOption) {
		this.embedded = embeddedCLIOption;
	}

	/** Put the string representation of the properties of this object into the given buffer.
	 *
	 * @param buffer the buffer.
	 */
	public void buildPropertyString(StringBuilder buffer) {
		buffer.append("hideLogo = ").append(this.hideLogo).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("hideInfo = ").append(this.hideInfo).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("showLogo = ").append(this.showInfo).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("defaultContextId = ").append(this.defaultContextId).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("randomContextId = ").append(this.randomContextId).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("bootAgentContextId = ").append(this.bootAgentContextId).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("offline = ").append(this.offline).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("embedded = ").append(this.embedded).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("noMoreOption = ").append(this.noMoreOption).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
	}

}
