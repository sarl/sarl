/**
 * 
 */
package io.sarl.eclipse.navigator.node;

import io.sarl.eclipse.images.EclipseSARLImages;
import io.sarl.eclipse.navigator.ISARLProjectElement;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.graphics.Image;

/**
 * The node of SARL Project called SARL just below the root node
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectSARLNode implements ISARLProjectElement {


	private static final String NAME = "SARL"; //$NON-NLS-1$
	
	private ISARLProjectElement parent;
	private ISARLProjectElement[] children;
	private Image image;

	/** Construct a SARL node.
	 * 
	 * @param iparent - the parent node.
	 */
	public SARLProjectSARLNode(ISARLProjectElement iparent) {
		this.parent = iparent;
	}

    @Override
	public Image getImage() {
		if (this.image == null) {
			this.image = EclipseSARLImages.getImage("icons/sarl-project-schema-16.png"); //$NON-NLS-1$
		}
		return this.image;
	}

    @Override
	public Object[] getChildren() {
		if (this.children == null) {
			this.children = initializeChildren(/*getProject()*/);
		}
		// else the children are just fine

		return this.children;
	}

    @Override
	public String getText() {
		return NAME;
	}

    @Override
	public boolean hasChildren() {
		if (this.children == null) {
			this.children = initializeChildren(/*getProject()*/);
		}
		// else we have already initialized them

		return this.children.length > 0;
	}
	
    private static ISARLProjectElement[] initializeChildren() {
		ISARLProjectElement[] ichildren = new ISARLProjectElement[0];
		return ichildren;
    }

    @Override
	public IProject getProject() {
		return this.parent.getProject();
	}

    @Override
	public Object getParent() {
		return this.parent;
	}

}
