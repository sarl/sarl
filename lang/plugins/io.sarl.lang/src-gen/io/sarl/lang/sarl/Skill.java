/**
 */
package io.sarl.lang.sarl;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Skill</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.Skill#getImplementedCapacities <em>Implemented Capacities</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.Skill#getFeatures <em>Features</em>}</li>
 * </ul>
 * </p>
 *
 * @see io.sarl.lang.sarl.SarlPackage#getSkill()
 * @model
 * @generated
 */
public interface Skill extends AbstractElement
{
  /**
   * Returns the value of the '<em><b>Implemented Capacities</b></em>' reference list.
   * The list contents are of type {@link io.sarl.lang.sarl.Capacity}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Implemented Capacities</em>' reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Implemented Capacities</em>' reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getSkill_ImplementedCapacities()
   * @model
   * @generated
   */
  EList<Capacity> getImplementedCapacities();

  /**
   * Returns the value of the '<em><b>Features</b></em>' containment reference list.
   * The list contents are of type {@link io.sarl.lang.sarl.SkillFeature}.
   * <!-- begin-user-doc -->
   * <p>
   * If the meaning of the '<em>Features</em>' containment reference list isn't clear,
   * there really should be more of a description here...
   * </p>
   * <!-- end-user-doc -->
   * @return the value of the '<em>Features</em>' containment reference list.
   * @see io.sarl.lang.sarl.SarlPackage#getSkill_Features()
   * @model containment="true"
   * @generated
   */
  EList<SkillFeature> getFeatures();

} // Skill
