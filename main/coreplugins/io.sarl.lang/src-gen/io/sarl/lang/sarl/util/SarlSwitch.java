/**
 */
package io.sarl.lang.sarl.util;

import io.sarl.lang.sarl.*;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.util.Switch;

import org.eclipse.xtend.core.xtend.XtendAnnotationTarget;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendEnum;
import org.eclipse.xtend.core.xtend.XtendEnumLiteral;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;

import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;

/**
 * <!-- begin-user-doc -->
 * The <b>Switch</b> for the model's inheritance hierarchy.
 * It supports the call {@link #doSwitch(EObject) doSwitch(object)}
 * to invoke the <code>caseXXX</code> method for each class of the model,
 * starting with the actual class of the object
 * and proceeding up the inheritance hierarchy
 * until a non-null result is returned,
 * which is the result of the switch.
 * <!-- end-user-doc -->
 * @see io.sarl.lang.sarl.SarlPackage
 * @generated
 */
public class SarlSwitch<T> extends Switch<T>
{
	/**
	 * The cached model package
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected static SarlPackage modelPackage;

	/**
	 * Creates an instance of the switch.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public SarlSwitch()
	{
		if (modelPackage == null)
		{
			modelPackage = SarlPackage.eINSTANCE;
		}
	}

	/**
	 * Checks whether this is a switch for the given package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param ePackage the package in question.
	 * @return whether this is a switch for the given package.
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage)
	{
		return ePackage == modelPackage;
	}

	/**
	 * Calls <code>caseXXX</code> for each class of the model until one returns a non null result; it yields that result.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the first non-null result returned by a <code>caseXXX</code> call.
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject)
	{
		switch (classifierID)
		{
			case SarlPackage.SARL_SCRIPT:
			{
				SarlScript sarlScript = (SarlScript)theEObject;
				T result = caseSarlScript(sarlScript);
				if (result == null) result = caseXtendFile(sarlScript);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_FIELD:
			{
				SarlField sarlField = (SarlField)theEObject;
				T result = caseSarlField(sarlField);
				if (result == null) result = caseXtendField(sarlField);
				if (result == null) result = caseXtendMember(sarlField);
				if (result == null) result = caseXtendAnnotationTarget(sarlField);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_BREAK_EXPRESSION:
			{
				SarlBreakExpression sarlBreakExpression = (SarlBreakExpression)theEObject;
				T result = caseSarlBreakExpression(sarlBreakExpression);
				if (result == null) result = caseXExpression(sarlBreakExpression);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_CONTINUE_EXPRESSION:
			{
				SarlContinueExpression sarlContinueExpression = (SarlContinueExpression)theEObject;
				T result = caseSarlContinueExpression(sarlContinueExpression);
				if (result == null) result = caseXExpression(sarlContinueExpression);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_ASSERT_EXPRESSION:
			{
				SarlAssertExpression sarlAssertExpression = (SarlAssertExpression)theEObject;
				T result = caseSarlAssertExpression(sarlAssertExpression);
				if (result == null) result = caseXExpression(sarlAssertExpression);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_ACTION:
			{
				SarlAction sarlAction = (SarlAction)theEObject;
				T result = caseSarlAction(sarlAction);
				if (result == null) result = caseXtendFunction(sarlAction);
				if (result == null) result = caseXtendExecutable(sarlAction);
				if (result == null) result = caseXtendMember(sarlAction);
				if (result == null) result = caseXtendAnnotationTarget(sarlAction);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_CONSTRUCTOR:
			{
				SarlConstructor sarlConstructor = (SarlConstructor)theEObject;
				T result = caseSarlConstructor(sarlConstructor);
				if (result == null) result = caseXtendConstructor(sarlConstructor);
				if (result == null) result = caseXtendExecutable(sarlConstructor);
				if (result == null) result = caseXtendMember(sarlConstructor);
				if (result == null) result = caseXtendAnnotationTarget(sarlConstructor);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_BEHAVIOR_UNIT:
			{
				SarlBehaviorUnit sarlBehaviorUnit = (SarlBehaviorUnit)theEObject;
				T result = caseSarlBehaviorUnit(sarlBehaviorUnit);
				if (result == null) result = caseXtendMember(sarlBehaviorUnit);
				if (result == null) result = caseXtendAnnotationTarget(sarlBehaviorUnit);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_CAPACITY_USES:
			{
				SarlCapacityUses sarlCapacityUses = (SarlCapacityUses)theEObject;
				T result = caseSarlCapacityUses(sarlCapacityUses);
				if (result == null) result = caseXtendMember(sarlCapacityUses);
				if (result == null) result = caseXtendAnnotationTarget(sarlCapacityUses);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_REQUIRED_CAPACITY:
			{
				SarlRequiredCapacity sarlRequiredCapacity = (SarlRequiredCapacity)theEObject;
				T result = caseSarlRequiredCapacity(sarlRequiredCapacity);
				if (result == null) result = caseXtendMember(sarlRequiredCapacity);
				if (result == null) result = caseXtendAnnotationTarget(sarlRequiredCapacity);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_CLASS:
			{
				SarlClass sarlClass = (SarlClass)theEObject;
				T result = caseSarlClass(sarlClass);
				if (result == null) result = caseXtendClass(sarlClass);
				if (result == null) result = caseXtendTypeDeclaration(sarlClass);
				if (result == null) result = caseXtendMember(sarlClass);
				if (result == null) result = caseXtendAnnotationTarget(sarlClass);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_INTERFACE:
			{
				SarlInterface sarlInterface = (SarlInterface)theEObject;
				T result = caseSarlInterface(sarlInterface);
				if (result == null) result = caseXtendInterface(sarlInterface);
				if (result == null) result = caseXtendTypeDeclaration(sarlInterface);
				if (result == null) result = caseXtendMember(sarlInterface);
				if (result == null) result = caseXtendAnnotationTarget(sarlInterface);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_ENUMERATION:
			{
				SarlEnumeration sarlEnumeration = (SarlEnumeration)theEObject;
				T result = caseSarlEnumeration(sarlEnumeration);
				if (result == null) result = caseXtendEnum(sarlEnumeration);
				if (result == null) result = caseXtendTypeDeclaration(sarlEnumeration);
				if (result == null) result = caseXtendMember(sarlEnumeration);
				if (result == null) result = caseXtendAnnotationTarget(sarlEnumeration);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_ANNOTATION_TYPE:
			{
				SarlAnnotationType sarlAnnotationType = (SarlAnnotationType)theEObject;
				T result = caseSarlAnnotationType(sarlAnnotationType);
				if (result == null) result = caseXtendAnnotationType(sarlAnnotationType);
				if (result == null) result = caseXtendTypeDeclaration(sarlAnnotationType);
				if (result == null) result = caseXtendMember(sarlAnnotationType);
				if (result == null) result = caseXtendAnnotationTarget(sarlAnnotationType);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_ENUM_LITERAL:
			{
				SarlEnumLiteral sarlEnumLiteral = (SarlEnumLiteral)theEObject;
				T result = caseSarlEnumLiteral(sarlEnumLiteral);
				if (result == null) result = caseXtendEnumLiteral(sarlEnumLiteral);
				if (result == null) result = caseXtendMember(sarlEnumLiteral);
				if (result == null) result = caseXtendAnnotationTarget(sarlEnumLiteral);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_EVENT:
			{
				SarlEvent sarlEvent = (SarlEvent)theEObject;
				T result = caseSarlEvent(sarlEvent);
				if (result == null) result = caseXtendTypeDeclaration(sarlEvent);
				if (result == null) result = caseXtendMember(sarlEvent);
				if (result == null) result = caseXtendAnnotationTarget(sarlEvent);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_CASTED_EXPRESSION:
			{
				SarlCastedExpression sarlCastedExpression = (SarlCastedExpression)theEObject;
				T result = caseSarlCastedExpression(sarlCastedExpression);
				if (result == null) result = caseXCastedExpression(sarlCastedExpression);
				if (result == null) result = caseXExpression(sarlCastedExpression);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_SPACE:
			{
				SarlSpace sarlSpace = (SarlSpace)theEObject;
				T result = caseSarlSpace(sarlSpace);
				if (result == null) result = caseXtendTypeDeclaration(sarlSpace);
				if (result == null) result = caseXtendMember(sarlSpace);
				if (result == null) result = caseXtendAnnotationTarget(sarlSpace);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_ARTIFACT:
			{
				SarlArtifact sarlArtifact = (SarlArtifact)theEObject;
				T result = caseSarlArtifact(sarlArtifact);
				if (result == null) result = caseXtendTypeDeclaration(sarlArtifact);
				if (result == null) result = caseXtendMember(sarlArtifact);
				if (result == null) result = caseXtendAnnotationTarget(sarlArtifact);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_AGENT:
			{
				SarlAgent sarlAgent = (SarlAgent)theEObject;
				T result = caseSarlAgent(sarlAgent);
				if (result == null) result = caseXtendTypeDeclaration(sarlAgent);
				if (result == null) result = caseXtendMember(sarlAgent);
				if (result == null) result = caseXtendAnnotationTarget(sarlAgent);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_CAPACITY:
			{
				SarlCapacity sarlCapacity = (SarlCapacity)theEObject;
				T result = caseSarlCapacity(sarlCapacity);
				if (result == null) result = caseXtendTypeDeclaration(sarlCapacity);
				if (result == null) result = caseXtendMember(sarlCapacity);
				if (result == null) result = caseXtendAnnotationTarget(sarlCapacity);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_BEHAVIOR:
			{
				SarlBehavior sarlBehavior = (SarlBehavior)theEObject;
				T result = caseSarlBehavior(sarlBehavior);
				if (result == null) result = caseXtendTypeDeclaration(sarlBehavior);
				if (result == null) result = caseXtendMember(sarlBehavior);
				if (result == null) result = caseXtendAnnotationTarget(sarlBehavior);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_SKILL:
			{
				SarlSkill sarlSkill = (SarlSkill)theEObject;
				T result = caseSarlSkill(sarlSkill);
				if (result == null) result = caseXtendTypeDeclaration(sarlSkill);
				if (result == null) result = caseXtendMember(sarlSkill);
				if (result == null) result = caseXtendAnnotationTarget(sarlSkill);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			case SarlPackage.SARL_FORMAL_PARAMETER:
			{
				SarlFormalParameter sarlFormalParameter = (SarlFormalParameter)theEObject;
				T result = caseSarlFormalParameter(sarlFormalParameter);
				if (result == null) result = caseXtendParameter(sarlFormalParameter);
				if (result == null) result = caseXtendAnnotationTarget(sarlFormalParameter);
				if (result == null) result = defaultCase(theEObject);
				return result;
			}
			default: return defaultCase(theEObject);
		}
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Script</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Script</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlScript(SarlScript object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Field</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Field</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlField(SarlField object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Break Expression</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Break Expression</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @since 0.5
	 * @generated
	 */
	public T caseSarlBreakExpression(SarlBreakExpression object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Continue Expression</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Continue Expression</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @since 0.7
	 * @generated
	 */
	public T caseSarlContinueExpression(SarlContinueExpression object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Assert Expression</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Assert Expression</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @since 0.6
	 * @generated
	 */
	public T caseSarlAssertExpression(SarlAssertExpression object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Action</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Action</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlAction(SarlAction object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Constructor</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Constructor</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlConstructor(SarlConstructor object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Behavior Unit</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Behavior Unit</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlBehaviorUnit(SarlBehaviorUnit object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Capacity Uses</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Capacity Uses</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlCapacityUses(SarlCapacityUses object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Required Capacity</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Required Capacity</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlRequiredCapacity(SarlRequiredCapacity object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Class</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Class</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlClass(SarlClass object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Interface</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Interface</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlInterface(SarlInterface object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Enumeration</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Enumeration</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlEnumeration(SarlEnumeration object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Annotation Type</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Annotation Type</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlAnnotationType(SarlAnnotationType object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Enum Literal</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Enum Literal</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlEnumLiteral(SarlEnumLiteral object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Event</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Event</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlEvent(SarlEvent object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Casted Expression</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Casted Expression</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @since 0.9
	 * @generated
	 */
	public T caseSarlCastedExpression(SarlCastedExpression object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Space</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Space</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlSpace(SarlSpace object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Artifact</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Artifact</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlArtifact(SarlArtifact object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Agent</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Agent</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlAgent(SarlAgent object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Capacity</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Capacity</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlCapacity(SarlCapacity object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Behavior</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Behavior</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlBehavior(SarlBehavior object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Skill</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Skill</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlSkill(SarlSkill object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Formal Parameter</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Formal Parameter</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseSarlFormalParameter(SarlFormalParameter object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>File</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>File</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendFile(XtendFile object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Annotation Target</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Annotation Target</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendAnnotationTarget(XtendAnnotationTarget object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Member</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Member</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendMember(XtendMember object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Field</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Field</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendField(XtendField object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>XExpression</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>XExpression</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @since 2.7
	 * @generated
	 */
	public T caseXExpression(XExpression object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Executable</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Executable</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendExecutable(XtendExecutable object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Function</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Function</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendFunction(XtendFunction object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Constructor</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Constructor</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendConstructor(XtendConstructor object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Type Declaration</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Type Declaration</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendTypeDeclaration(XtendTypeDeclaration object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Class</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Class</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendClass(XtendClass object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Interface</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Interface</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendInterface(XtendInterface object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Enum</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Enum</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendEnum(XtendEnum object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Annotation Type</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Annotation Type</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendAnnotationType(XtendAnnotationType object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Enum Literal</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Enum Literal</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendEnumLiteral(XtendEnumLiteral object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>XCasted Expression</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>XCasted Expression</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @since 2.7
	 * @generated
	 */
	public T caseXCastedExpression(XCastedExpression object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>Parameter</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>Parameter</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	public T caseXtendParameter(XtendParameter object)
	{
		return null;
	}

	/**
	 * Returns the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * <!-- begin-user-doc -->
	 * This implementation returns null;
	 * returning a non-null result will terminate the switch, but this is the last case anyway.
	 * <!-- end-user-doc -->
	 * @param object the target of the switch.
	 * @return the result of interpreting the object as an instance of '<em>EObject</em>'.
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object)
	{
		return null;
	}

} //SarlSwitch
