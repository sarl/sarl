/**
 * 
 */
package io.sarl.eclipse.navigator;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.graphics.Image;

/**
 * A SARL Project Element.
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface ISARLProjectElement {
	
	/** Replies the image associated to the project element.
	 * 
	 * @return the image.
	 */
    public Image getImage();
    
	/** Replies the subelements the project element.
	 * 
	 * @return the subelements.
	 */
    public Object[] getChildren();
 
	/** Replies the text associated to the project element.
	 * 
	 * @return the text.
	 */
    public String getText();
 
	/** Replies if this project element has a subelement.
	 * 
	 * @return <code>true</code> if a subelement is existing,
	 * <code>false</code> otherwise.
	 */
    public boolean hasChildren();
 
	/** Replies the project in which this project element is
	 * located.
	 * 
	 * @return the root project.
	 */
    public IProject getProject();
 
	/** Replies the element that is containing this project
	 * element
	 * 
	 * @return the parent element.
	 */
    public Object getParent();
    
}
