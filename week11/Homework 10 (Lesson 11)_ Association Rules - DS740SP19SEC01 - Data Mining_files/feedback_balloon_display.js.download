function showAtomFeedback( event, labelId, contents ) {
	var label = UI.GetById( labelId );
	fbBalloon.contents.innerHTML = contents;
	fbBalloon.Show( event, label );
}
function showDropDownFeedbackForSelectList( obj, objName ) {
	var feedback = UI.GetByName( objName + '_' + obj.GetSelectedValue() + '_id' );
	if( feedback) {
		fbBalloon.contents.innerHTML = feedback.value;
		fbBalloon.Show( null, obj );
	}
}
function showDropDownFeedback( event, obj ) {
	var feedback = UI.GetByName( obj.name + '_' + obj.GetSelectedValue() + '_id' );
	if( feedback) {
		fbBalloon.contents.innerHTML = feedback.value;
		fbBalloon.Show( event, obj );
	}
}